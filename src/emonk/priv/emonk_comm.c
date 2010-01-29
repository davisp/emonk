
#include "emonk.h"
#include "emonk_comm.h"

inline int
read_int32(unsigned char* data, int* index)
{
    *index += 4;
    return (((int) data[0]) << 24)
         | (((int) data[1]) << 16)
         | (((int) data[2]) << 8)
         | (((int) data[3]) << 0);
}

inline int
read_int16(unsigned char* data, int* index)
{
    *index += 2;
    return (((int) data[0]) << 8)
         | (((int) data[1]) << 0);
}

inline void
write_int32(unsigned char* data, int val)
{
    data[0] = (val >> 24) & 0xFF;
    data[1] = (val >> 16) & 0xFF;
    data[2] = (val >> 8) & 0xFF;
    data[3] = val & 0xFF;
}

emonk_req_t*
read_req_info(JSContext* cx, uint cmd, unsigned char* buf, int len)
{
    emonk_req_t* req = NULL;
    int idx = 0;
    int funclen;
    int remain;
    int i;

    BEGIN_REQ(cx);
    req = (emonk_req_t*) driver_alloc(sizeof(emonk_req_t));
    if(req == NULL) goto error;
    memset(req, 0, sizeof(emonk_req_t));
        
    // Parse the enclosing structure.
    if(buf[idx++] != VERSION_MAGIC) goto error;
    if(buf[idx++] != SMALL_TUPLE) goto error;
    if(buf[idx] != 2) goto error;
    idx += 1;

    // Parse call token
    if(len-idx < 5) goto error;
    if(buf[idx++] != BINARY) goto error;
    req->cid_len = read_int32(buf+idx, &idx);
    req->call_id = buf + idx;
    idx += req->cid_len;

    if(cmd == 0) // Reading a script
    {
        if(len-idx < 5) goto error;
        if(buf[idx++] != BINARY) goto error;
        req->scr_len = read_int32(buf+idx, &idx);
        req->script = buf + idx;
        idx += req->scr_len;
        
        END_REQ(cx);
        return req;
    }
    else // Reading a function signature.
    {
        if(buf[idx++] != SMALL_TUPLE) goto error;
        if(buf[idx++] != 2) goto error;
        if(buf[idx++] != BINARY) goto error;
        funclen = read_int32(buf+idx, &idx);
        
        // JS_GetProperty wants a null-terminated string
        // so we need to copy out to ensure that we are.
        req->function = (char*) driver_alloc(funclen+1);
        if(req->function == NULL) goto error;
        memcpy(req->function, buf+idx, funclen);
        req->function[funclen] = '\0';
        idx += funclen;
                
        if(buf[idx] == NIL)
        {
            req->argc = 0;
            END_REQ(cx);
            return req;
        }
        
        if(buf[idx] == STRING)
        {
            idx += 1;
            req->argc = read_int16(buf+idx, &idx);
            req->argv = (jsval*) driver_alloc(req->argc * sizeof(jsval));
            if(req->argv == NULL) goto error;
            for(i = 0; i < req->argc; i++)
            {
                req->argv[i] = INT_TO_JSVAL((int) buf[idx+i]);
            }
            idx += req->argc;
        }
        else if(buf[idx++] == LIST)
        {
            req->argc = read_int32(buf+idx, &idx);
            req->argv = (jsval*) driver_alloc(req->argc * sizeof(jsval));
            if(req->argv == NULL) goto error;

            remain = len - idx;
            for(i = 0; i < req->argc && remain > 0; i++)
            {
                req->argv[i] = to_js(cx, buf+idx, &remain);
                if(req->argv[i] == JSVAL_VOID)
                {
                    goto error;
                }
            }
            if(i < req->argc) goto error;
        }
        else
        {
            goto error;
        }

        END_REQ(cx);
        return req;
    }

error:
    END_REQ(cx);
    return free_req_info(req);
}

void*
free_req_info(emonk_req_t* req)
{
    if(req == NULL) return NULL;
    if(req->function != NULL) driver_free(req->function);
    if(req->argv != NULL) driver_free(req->argv);
    if(req != NULL) driver_free(req);
    return NULL;
}

void
mk_error(JSContext* cx, const char* mesg, JSErrorReport* report)
{
    ErlDrvBinary* bin;
    unsigned char* data;
    int len = 0;
    int pos = 0;
    int tmp;
    
    if(!(report->flags & JSREPORT_EXCEPTION)) return;

    if(mesg == NULL) mesg = "";
    if(report->linebuf == NULL) report->linebuf = "";

    // Length calculation:
    // magic = 1
    // tuple = 1+1
    // mesg = 1+4+len(mesg)
    // linenum = 1+4
    // line = 1+4+len(src)
    
    len = 18 + strlen(mesg) + strlen(report->linebuf);

    bin = driver_alloc_binary(len);
    if(bin == NULL) return;
    data = (unsigned char*) bin->orig_bytes;
    
    
    data[pos++] = VERSION_MAGIC;
    data[pos++] = SMALL_TUPLE;
    data[pos++] = 3;

    data[pos++] = BINARY;
    tmp = strlen(mesg);
    write_int32(data+pos, tmp);
    pos += 4;
    memcpy(data+pos, mesg, tmp);
    pos += tmp;
    
    data[pos++] = INTEGER;
    tmp = report->lineno;
    write_int32(data+pos, tmp);
    pos += 4;
    
    data[pos++] = BINARY;
    tmp = strlen(report->linebuf);
    write_int32(data+pos, tmp);
    pos += 4;
    memcpy(data+pos, report->linebuf, tmp);
    pos += tmp;

    JS_SetContextPrivate(cx, bin);
    
    return;
}