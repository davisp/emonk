
#include "emonk_comm.h"

#define MIN(a, b) ((a) < (b) ? (a) : (b))

jschar* to_js_key(JSContext* cx, char* data, int* remaining, size_t* charslen);
jsval to_js_object(JSContext* cx, char* data, int* remaining);

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
    
    return JSVAL_VOID;
}