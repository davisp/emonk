
#include "emonk_comm.h"

typedef struct _emonk_buf_t
{
    unsigned char* buf;
    int size;
    int used;
} emonk_buf_t;

#define ERROR 0
#define OK 1
#define IGNORE 2
#define BUFPTR (buf->buf+buf->used)
#define REQUEST(len) {if(!ensure_space(buf, (len))) return ERROR;}

int to_erl_intern(emonk_buf_t* buf, JSContext* cx, jsval val);

int
ensure_space(emonk_buf_t* buf, int request)
{
    char* tmp;
    if(request < buf->size - buf->used) return OK;
    
    while(buf->size < buf->used + request) buf->size *= 2;
    tmp = driver_realloc(buf->buf, buf->size);
    if(tmp == NULL) return ERROR;
    buf->buf = tmp;

    return OK;
}

int
to_erl_atom(emonk_buf_t* buf, char* data, unsigned short len)
{
    unsigned short nlen = htons(len);
    REQUEST(3+len);

    BUFPTR[0] = ATOM;
    memcpy(BUFPTR+1, &nlen, 2);
    memcpy(BUFPTR+3, data, len);
    buf->used += 3 + len;

    return OK;
}

int
to_erl_string(emonk_buf_t* buf, JSContext* cx, jsval val)
{
    JSString *str;
    const char* data;
    size_t len, netlen;

    str = JS_ValueToString(cx, val);
    data = JS_GetStringBytesZ(cx, str);
    if(data == NULL) return ERROR;
    len = strlen(data);
    
    REQUEST(5+len);

    BUFPTR[0] = BINARY;
    netlen = htonl(len);
    memcpy(BUFPTR+1, &netlen, 4);
    memcpy(BUFPTR+5, data, len);
    buf->used += 5 + len;

    return OK;
}

int
to_erl_int(emonk_buf_t* buf, JSContext* cx, jsval val)
{
    int32_t rval;
    
    if(!JS_ValueToInt32(cx, val, &rval)) return ERROR;
    
    if((unsigned int) rval < 255)
    {
        REQUEST(2);
        BUFPTR[0] = SMALL_INTEGER;
        BUFPTR[1] = (char) rval;
        buf->used += 2;
    }
    else
    {
        REQUEST(5);
        BUFPTR[0] = INTEGER;
        rval = htonl(rval);
        memcpy(BUFPTR+1, &rval, 4);
        buf->used += 5;
    }

    return OK;    
}

int
to_erl_float(emonk_buf_t* buf, JSContext* cx, jsval val)
{
    double rval;
    
    if(!JS_ValueToNumber(cx, val, &rval)) return ERROR;

    REQUEST(32);
    BUFPTR[0] = FLOAT;
    snprintf(BUFPTR+1, 31, "%.20e", rval);
    buf->used += 32;

    return OK;
}

int
to_erl_array(emonk_buf_t* buf, JSContext* cx, JSObject* obj)
{
    unsigned int length, nlen;
    jsval v;
    int i;
    
    if(!JS_GetArrayLength(cx, obj, &length)) return ERROR;
    
    REQUEST(5);

    BUFPTR[0] = LIST;
    nlen = htonl(length);
    memcpy(BUFPTR+1, &nlen, 4);
    buf->used += 5;

    for(i = 0; i < length; i++)
    {
        if(!JS_GetElement(cx, obj, i, &v)) return ERROR;
        if(!to_erl_intern(buf, cx, v)) return ERROR;
    }

    REQUEST(1);
    BUFPTR[0] = NIL;
    buf->used += 1;
    
    return OK;
}

int
to_erl_object(emonk_buf_t* buf, JSContext* cx, JSObject* obj)
{
    JSObject* iter;
    JSString* key;
    jsid idp;
    jsval val;
    jschar* keyname;
    size_t keylen;
    int count = 0;
    int lengthpos;
    
    REQUEST(7);

    BUFPTR[0] = SMALL_TUPLE;
    BUFPTR[1] = (char) 1;
    BUFPTR[2] = LIST;
    
    // Remember the byte offset where length goes so we can write it
    // after enumerating the properties.
    lengthpos = buf->used + 3;
    buf->used += 7;
    
    iter = JS_NewPropertyIterator(cx, obj);
    if(iter == NULL) return ERROR;
    
    while(JS_NextProperty(cx, iter, &idp))
    {
        // Done iterating, write length and bail.
        if(idp == JSVAL_VOID)
        {
            count = htonl(count);
            memcpy(buf->buf+lengthpos, &count, 4);

            REQUEST(1);
            BUFPTR[0] = NIL;
            buf->used += 1;
            
            return OK;
        }

        REQUEST(2);
        BUFPTR[0] = SMALL_TUPLE;
        BUFPTR[1] = 2;
        buf->used += 2;

        if(!JS_IdToValue(cx, idp, &val)) return ERROR;
        if(!to_erl_string(buf, cx, val)) return ERROR;
        
        key = JS_ValueToString(cx, val);
        keyname = JS_GetStringChars(key);
        keylen = JS_GetStringLength(key);
        
        if(!JS_GetUCProperty(cx, obj, keyname, keylen, &val)) return ERROR;
        if(!to_erl_intern(buf, cx, val)) return ERROR;
        count += 1;
    }

    return ERROR;
}

int
to_erl_from_handler(emonk_buf_t* buf, JSContext* cx, JSObject* obj)
{
    JSObject* func;
    jsval tojson;
    jsval rval;
    
    if(!JS_GetProperty(cx, obj, "toJSON", &tojson))
    {
        return ERROR;
    }
    
    if(!JSVAL_IS_OBJECT(tojson)) return IGNORE;
    func = JSVAL_TO_OBJECT(tojson);
    if(func == NULL) return ERROR;
    if(!JS_ObjectIsFunction(cx, func)) return IGNORE;

    if(!JS_CallFunctionValue(cx, obj, tojson, 0, NULL, &rval))
    {
        return ERROR;
    }
    
    return to_erl_intern(buf, cx, rval);
}

int
to_erl_intern(emonk_buf_t* buf, JSContext* cx, jsval val)
{
    JSObject* obj = NULL;
    JSType type = JS_TypeOfValue(cx, val);
    int status = ERROR;
        
    if(val == JSVAL_NULL)
    {
        return to_erl_atom(buf, "null", 4);
    }
    else if(val == JSVAL_VOID)
    {
        JS_ReportError(cx, "Cannot encode 'undefined' value as JSON");
        return ERROR;
    }
    else if(type == JSTYPE_BOOLEAN)
    {
        if(val == JSVAL_TRUE)
            return to_erl_atom(buf, "true", 4);
        else
            return to_erl_atom(buf, "false", 5);
    }
    else if(type == JSTYPE_STRING)
    {
        return to_erl_string(buf, cx, val);
    }
    else if(type == JSTYPE_XML)
    {
        return to_erl_string(buf, cx, val);
    }
    else if(type == JSTYPE_NUMBER)
    {
        if(JSVAL_IS_INT(val))
            return to_erl_int(buf, cx, val);
        else
            return to_erl_float(buf, cx, val);
    }
    else if(type == JSTYPE_OBJECT)
    {
        obj = JSVAL_TO_OBJECT(val);
        status = to_erl_from_handler(buf, cx, obj);
        if(status != IGNORE) return status;
        
        if(JS_IsArrayObject(cx, obj))
        {
            return to_erl_array(buf, cx, obj);
        }
        return to_erl_object(buf, cx, obj);
    }
    
    return ERROR;
}

void*
to_erl(JSContext* cx, jsval val, int* length)
{
    emonk_buf_t buf;
    char* ret;
    
    buf.size = 1024;
    buf.used = 1; // VERSION_MAGIC
    buf.buf = (unsigned char*) driver_alloc(buf.size * sizeof(char));
    if(buf.buf == NULL) return NULL;
    buf.buf[0] = VERSION_MAGIC;

    if(!to_erl_intern(&buf, cx, val))
    {
        driver_free(buf.buf);
        return NULL;
    }

    ret = driver_realloc(buf.buf, buf.used);
    if(ret == NULL)
    {
        if(buf.buf != NULL) driver_free(buf.buf);
        return NULL;
    }
    
    *length = buf.used;
    return ret;
}
