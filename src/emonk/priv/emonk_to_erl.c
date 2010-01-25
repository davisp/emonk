
#include "emonk_comm.h"

char* to_erl_intern(JSContext* cx, jsval val, char* buf, int* size, int* used);

char*
ensure_space(char* buf, int* size, int* used, int request)
{
    if(request < *used)
    {
        return buf;
    }
    
    while(*size < *used + request) *size *= 2;
    buf = driver_realloc(buf, *size);
    return buf;
}

char*
to_erl_atom(char* buf, int* size, int* used, char* data, unsigned short len)
{
    unsigned short nlen = htons(len);
    data = ensure_space(buf, size, used, 3 + len);
    if(buf == NULL) return NULL;
    buf[*used] = ATOM;
    memcpy(buf+(*used)+1, &nlen, 2);
    memcpy(buf+(*used)+3, data, len);
    *used += 3 + len;
    return buf;
}

char*
to_erl_string(JSContext* cx, jsval val, char* buf, int* size, int* used)
{
    JSString *str;
    const char* data;
    size_t len, netlen;

    str = JS_ValueToString(cx, val);
    data = JS_GetStringBytesZ(cx, str);
    if(data == NULL) return NULL;
    len = strlen(data);
    
    buf = ensure_space(buf, size, used, 5+len);
    if(buf == NULL) return NULL;

    buf[*used] = BINARY;
    netlen = htonl(len);
    memcpy(buf+(*used)+1, &netlen, 4);
    memcpy(buf+(*used)+5, data, len);

    *used += 5 + len;
    return buf;
}

char*
to_erl_int(JSContext* cx, jsval val, char* buf, int* size, int* used)
{
    int32_t rval;
    
    if(!JS_ValueToInt32(cx, val, &rval))
    {
        return NULL;
    }
    
    if((unsigned int) rval < 255)
    {
        buf = ensure_space(buf, size, used, 2);
        if(buf == NULL) return NULL;
        buf[*used] = SMALL_INTEGER;
        buf[(*used)+1] = (char) rval;
        *used += 2;
    }
    else
    {
        buf = ensure_space(buf, size, used, 5);
        if(buf == NULL) return NULL;
        buf[*used] = INTEGER;
        rval = htonl(rval);
        memcpy(buf+(*used)+1, &rval, 4);
        *used += 5;
    }

    return buf;    
}

char*
to_erl_float(JSContext* cx, jsval val, char* buf, int* size, int* used)
{
    double rval;
    
    if(!JS_ValueToNumber(cx, val, &rval))
    {
        return NULL;
    }
    
    buf = ensure_space(buf, size, used, 32);
    if(buf == NULL) return NULL;
    buf[*used] = FLOAT;
    snprintf(buf+(*used)+1, 31, "%.20e", rval);
    *used += 32;
    return buf;
}

char*
to_erl_array(JSContext* cx, JSObject* obj, char* buf, int* size, int* used)
{
    unsigned int length, nlen;
    jsval v;
    int i;
    
    if(!JS_GetArrayLength(cx, obj, &length)) return NULL;
    
    buf = ensure_space(buf, size, used, 5);
    if(buf == NULL) return NULL;

    buf[*used] = LIST;
    nlen = htonl(length);
    memcpy(buf+(*used)+1, &nlen, 4);
    *used += 5;

    for(i = 0; i < length; i++)
    {
        if(!JS_GetElement(cx, obj, i, &v))
        {
            return NULL;
        }
        
        buf = to_erl_intern(cx, v, buf, size, used);
        if(buf == NULL) return NULL;
    }
    
    buf = ensure_space(buf, size, used, 1);
    if(buf == NULL) return NULL;
    buf[*used] = NIL;
    *used += 1;
    
    return buf;
}

char*
to_erl_object(JSContext* cx, JSObject* obj, char* buf, int* size, int *used)
{
    JSObject* iter;
    JSString* key;
    jsid idp;
    jsval val;
    jschar* keyname;
    size_t keylen;
    int count = 0;
    int lengthpos;
    
    buf = ensure_space(buf, size, used, 7);
    if(buf == NULL) return NULL;
    
    buf[*used] = SMALL_TUPLE;
    buf[(*used) + 1] = (char) 1;
    buf[(*used) + 2] = LIST;
    
    // Remember the byte offset where length goes so we can write it
    // after enumerating the properties.
    lengthpos = *used + 3;
    *used += 7;
    
    iter = JS_NewPropertyIterator(cx, obj);
    if(iter == NULL) return NULL;
    
    while(JS_NextProperty(cx, iter, &idp))
    {
        // Done iterating, write length and bail.
        if(idp == JSVAL_VOID)
        {
            count = htonl(count);
            memcpy(buf+lengthpos, &count, 4);
            
            buf = ensure_space(buf, size, used, 1);
            if(buf == NULL) return NULL;
            buf[*used] = NIL;
            *used += 1;
            
            return buf;
        }

        buf = ensure_space(buf, size, used, 2);
        buf[*used] = SMALL_TUPLE;
        buf[(*used) + 1] = 2;
        *used += 2;

        if(!JS_IdToValue(cx, idp, &val)) return NULL;

        buf = to_erl_string(cx, val, buf, size, used);
        if(buf == NULL) return NULL;
        
        key = JS_ValueToString(cx, val);
        keyname = JS_GetStringChars(key);
        keylen = JS_GetStringLength(key);
        
        if(!JS_GetUCProperty(cx, obj, keyname, keylen, &val)) return NULL;
        
        buf = to_erl_intern(cx, val, buf, size, used);
        count += 1;
    }
    
    // ERROR GETTING NEXT PROPERTY
    return NULL;
}

char*
to_erl_intern(JSContext* cx, jsval val, char* buf, int* size, int* used)
{
    JSObject* obj = NULL;
    JSType type = JS_TypeOfValue(cx, val);
        
    if(val == JSVAL_NULL)
    {
        return to_erl_atom(buf, size, used, "null", 4);
    }
    else if(val == JSVAL_VOID)
    {
        JS_ReportError(cx, "Cannot encode 'undefined' value as JSON");
        return NULL;
    }
    else if(type == JSTYPE_BOOLEAN)
    {
        if(val == JSVAL_TRUE)
            return to_erl_atom(buf, size, used, "true", 4);
        else
            return to_erl_atom(buf, size, used, "false", 5);
    }
    else if(type == JSTYPE_STRING)
    {
        return to_erl_string(cx, val, buf, size, used);
    }
    else if(type == JSTYPE_XML)
    {
        return to_erl_string(cx, val, buf, size, used);
    }
    else if(type == JSTYPE_NUMBER)
    {
        if(JSVAL_IS_INT(val))
            return to_erl_int(cx, val, buf, size, used);
        else
            return to_erl_float(cx, val, buf, size, used);
    }
    else if(type == JSTYPE_OBJECT)
    {
        obj = JSVAL_TO_OBJECT(val);
        if(JS_IsArrayObject(cx, obj))
        {
            return to_erl_array(cx, obj, buf, size, used);
        }
        return to_erl_object(cx, obj, buf, size, used);
    }
    
    return NULL;
}

void*
to_erl(JSContext* cx, jsval val, int* length)
{
    int size = 1024;
    char* buf = (char*) driver_alloc(size * sizeof(char));
    if(buf == NULL) return NULL;

    buf[0] = 131;
    *length = 1;
    return (void*) to_erl_intern(cx, val, buf, &size, length);
}
