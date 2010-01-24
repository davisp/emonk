#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <jsapi.h>
#include <erl_driver.h>

#define SMALL_INTEGER 'a'
#define INTEGER       'b'
#define FLOAT         'c'
#define ATOM          'd'
#define NEW_FLOAT     'F'
#define SMALL_TUPLE   'h'
#define LARGE_TUPLE   'i'
#define NIL           'j'
#define STRING        'k'
#define LIST          'l'
#define BINARY        'm'
#define SMALL_BIG     'n'
#define LARGE_BIG     'o'

#define VERSION_MAGIC 131

#define MIN(a, b) ((a) < (b) ? (a) : (b))

char* ensure_space(char* buf, int* size, int* used, int request);
char* to_erl_intern(JSContext* cx, jsval val, char* buf, int* size, int* used);
jschar* to_js_key(JSContext* cx, char* data, int* remaining, size_t* charslen);
jsval to_js_object(JSContext* cx, char* data, int* remaining);

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
    
    //fprintf(stderr, "CONVERTING TYPE: %d\n", (int) type);
    
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
    
    fprintf(stderr, "UNKNOWN JSVAL TYPE: %d\n", (int) type);
    return NULL;
}

void*
to_erl(JSContext* cx, jsval val, int* length)
{
    int size = 1024;
    char* buf = (char*) driver_alloc(size * sizeof(char));
    char* ret;
    if(buf == NULL) return NULL;

    buf[0] = 131;
    *length = 1;
    ret = to_erl_intern(cx, val, buf, &size, length);
    fprintf(stderr, "%p %p\n", buf, ret);
    return ret;
}

jsval
to_js_special(JSContext* cx, char* data, int* remaining)
{
    unsigned int length;
    int before;
    size_t charslen;
    jschar* chars;
    JSString* str;

    memcpy(&length, data, 2);
    length = ntohs(length);
    *remaining -= (2 + length);
    data += 2;
    
    if(strncmp(data, "true", MIN(length, 4)) == 0)
    {
        return JSVAL_TRUE;
    }
    else if(strncmp(data, "false", MIN(length, 5)) == 0)
    {
        return JSVAL_FALSE;
    }
    else if(strncmp(data, "null", MIN(length, 4)) == 0)
    {
        return JSVAL_NULL;
    }
    else
    {
        // Undo
        data -= 3;
        *remaining += (3+length);

        chars = to_js_key(cx, data, remaining, &charslen);
        if(chars == NULL) return JSVAL_VOID;

        str = JS_NewUCString(cx, chars, charslen);
        if(!str)
        {
            JS_free(cx, chars);
            return JSVAL_VOID;
        }

        return STRING_TO_JSVAL(str);
    }
}

jsval
to_js_int(JSContext* cx, char* data, int* remaining, char type)
{
    unsigned int val;
    jsval ret;

    if(type == SMALL_INTEGER)
    {
        val = (unsigned char) data[0];
        *remaining -= 1;
        return INT_TO_JSVAL(val);
    }
    else
    {
        memcpy(&val, data, 4);
        *remaining -= 4;
        val = ntohl(val);
        if(INT_FITS_IN_JSVAL(val))
        {
            return INT_TO_JSVAL(val);
        }
        else
        {
            if(!JS_NewNumberValue(cx, val, &ret))
            {
                return JSVAL_VOID;
            }
            
            return ret;
        }
    }
}

jsval
to_js_big_int(JSContext* cx, char* data, int* remaining, char type)
{
    // Note to self, check for overflow.
    
    jsval ret;
    int32_t val = 0;
    int length = 0;
    unsigned int factor;
    char sign = 0;
    int i;
        
    if(type == SMALL_BIG)
    {
        length = (unsigned char) data[0];
        *remaining -= 1;
        data += 1;
    }
    else
    {
        memcpy(&val, data, 4);
        val = ntohl(val);
        *remaining -= 4;
        data += 4;
    }
    
    sign = data[0];
    *remaining -= 1;
    data += 1;
    
    if(length > 4)
    {
        if(sign == 0)
        {
            return JS_GetPositiveInfinityValue(cx);
        }
        else
        {
            return JS_GetNegativeInfinityValue(cx);
        }
    }
    
    for(i = 0; i < length; i++)
    {
        factor = (unsigned int) data[i];
        val += factor << (i*8);
    }
    
    if(sign == 1) val *= -1;
        
    if(INT_FITS_IN_JSVAL(val))
    {
        return INT_TO_JSVAL(val);
    }
    else
    {
        if(!JS_NewNumberValue(cx, val, &ret))
        {
            return JSVAL_VOID;
        }
        
        return ret;
    }
}

jsval
to_js_float(JSContext* cx, char* data, int* remaining, char type)
{
    double val;
    jsval ret;
    
    if(type == NEW_FLOAT)
    {
        memcpy(&val, data, 8);
        *remaining -= 8;
    }
    else
    {
        sscanf(data, "%lf", &val);
        *remaining -= 31;
    }

    if(!JS_NewNumberValue(cx, val, &ret))
    {
        return JSVAL_VOID;
    }
    
    return ret;
}

jsval
to_js_string(JSContext* cx, char* data, int* remaining)
{
    int length;
    JSString* str;
    jschar* chars;
    size_t charslen;
    
    memcpy(&length, data, 4);
    length = ntohl(length);
    
    if(!JS_DecodeBytes(cx, data+4, length+1, NULL, &charslen))
    {
        return JSVAL_VOID;
    }
    
    chars = JS_malloc(cx, (charslen + 1) * sizeof(jschar));
    if(chars == NULL) return JSVAL_VOID;
    
    if(!JS_DecodeBytes(cx, data+4, length+1, chars, &charslen))
    {
        JS_free(cx, chars);
        return JSVAL_VOID;
    }
    chars[charslen] = '\0';
    
    str = JS_NewUCString(cx, chars, charslen - 1);
    if(!str)
    {
        JS_free(cx, chars);
        return JSVAL_VOID;
    }

    *remaining -= (4 + length);
    return STRING_TO_JSVAL(str);
}

jsval
to_js_array(JSContext* cx, char* data, int* remaining, char type)
{
    int length;
    unsigned int ulen;
    JSObject* ret;
    jsval val;
    int left;
    int i;

    if(type == STRING)
    {
        memcpy(&ulen, data, 2);
        length = ntohs(ulen);
        *remaining -= 2;
        data += 2;
    }
    else
    {
        memcpy(&length, data, 4);
        *remaining -= 4;
        data += 4;
        length = ntohl(length);
    }

    ret = JS_NewArrayObject(cx, 0, NULL);
    if(ret == NULL) return JSVAL_VOID;

    for(i = 0; i < length; i++)
    {
        if(type == STRING) // list of one byte integers.
        {
            val = to_js_int(cx, data, remaining, SMALL_INTEGER);
            data += 1;
        }
        else
        {
            left = *remaining;
            val = to_js_object(cx, data, remaining);
            data += (left - *remaining);
        }

        if(val == JSVAL_VOID) return val;
        JS_SetElement(cx, ret, i, &val);
    }
    
    if(type == LIST && data[0] != NIL)
    {
        return JSVAL_VOID;
    }
    else if(type == LIST)
    {
        *remaining -= 1;
    }
    
    return OBJECT_TO_JSVAL(ret);
}

jsval
to_js_empty_array(JSContext* cx, char* data, int* remaining)
{
    JSObject* ret = JS_NewArrayObject(cx, 0, NULL);
    if(ret == NULL) return JSVAL_VOID;
    return OBJECT_TO_JSVAL(ret);
}

jschar*
to_js_key(JSContext* cx, char* data, int* remaining, size_t* charslen)
{
    char type;
    size_t length;
    unsigned short atomlen;
    jschar* chars;
    
    type = data[0];
    *remaining -= 1;
    data += 1;
    
    if(type == BINARY)
    {
        memcpy(&length, data, 4);
        length = ntohl(length);
        *remaining -= 4;
        data += 4;
    }
    else if(type == ATOM)
    {
        memcpy(&atomlen, data, 2);
        atomlen = ntohs(atomlen);
        length = (size_t) atomlen;
        *remaining -= 2;
        data += 2;
    }
    else
    {
        return NULL;
    }
    
    if(!JS_DecodeBytes(cx, data, length, NULL, charslen))
    {
        return NULL;
    }
    
    chars = JS_malloc(cx, (*charslen + 1) * sizeof(jschar));
    if(chars == NULL) return NULL;
    
    if(!JS_DecodeBytes(cx, data, length, chars, charslen))
    {
        JS_free(cx, chars);
        return NULL;
    }
    chars[*charslen] = '\0';
    
    *remaining -= length;
    return chars;
}

jsval
to_js_object(JSContext* cx, char* data, int* remaining)
{
    int length;
    JSObject* ret;
    jschar* key;
    size_t klen;
    jsval val;
    int i;
    int before;
   
    if(data[0] != 1) return JSVAL_VOID;
    *remaining -= 1;
    data += 1;
        
    ret = JS_NewObject(cx, NULL, NULL, NULL);
    if(ret == NULL) return JSVAL_VOID;

    // Empty object
    if(data[0] == NIL)
    {
        *remaining -= 1;
        return OBJECT_TO_JSVAL(ret);
    }

    if(data[0] != LIST) return JSVAL_VOID;
    memcpy(&length, data+1, 4);
    *remaining -= 5;
    data += 5;
    length = ntohl(length);
    
    for(i = 0; i < length; i++)
    {
        if(data[0] != SMALL_TUPLE) return JSVAL_VOID;
        if(data[1] != 2) return JSVAL_VOID;
        *remaining -= 2;
        data += 2;

        before = *remaining;
        key = to_js_key(cx, data, remaining, &klen);
        if(key == NULL) return JSVAL_VOID;
        data += before - *remaining;
        
        before = *remaining;
        val = to_js_object(cx, data, remaining);
        if(val == JSVAL_VOID)
        {
            JS_free(cx, key);
            return JSVAL_VOID;
        }
        data += before - *remaining;
                
        if(!JS_SetUCProperty(cx, ret, key, klen, &val))
        {
            JS_free(cx, key);
            return JSVAL_VOID;
        }
        
        JS_free(cx, key);
    }
    
    if(data[0] != NIL)
    {
        fprintf(stderr, "NO END NIL FOR OBJECT LIST: %c\n", data[0]);
        return JSVAL_VOID;
    }
    *remaining -= 1;
    
    return OBJECT_TO_JSVAL(ret);
}

jsval
to_js(JSContext* cx, char* data, int* remaining)
{
    char type = data[0];
    data += 1;
    *remaining -= 1;
    
    fprintf(stderr, "MAKING OBJECT TYPE: '%c' %d\n", type, *remaining);
    
    if(type == ATOM)
    {
        return to_js_special(cx, data, remaining);
    }
    else if(type == SMALL_INTEGER || type == INTEGER)
    {
        return to_js_int(cx, data, remaining, type);
    }
    else if(type == SMALL_BIG || type == LARGE_BIG)
    {
        return to_js_big_int(cx, data, remaining, type);
    }
    else if(type == FLOAT || type == NEW_FLOAT)
    {
        return to_js_float(cx, data, remaining, FLOAT);
    }
    else if(type == BINARY)
    {
        return to_js_string(cx, data, remaining);
    }
    else if(type == STRING || type == LIST)
    {
        return to_js_array(cx, data, remaining, type);
    }
    else if(type == NIL)
    {
        return to_js_empty_array(cx, data, remaining);
    }
    else if(type == SMALL_TUPLE)
    {
        return to_js_object(cx, data, remaining);
    }
    
    fprintf(stderr, "INVALID TYPE: '%c'\n", type);
    return JSVAL_VOID;
}

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

