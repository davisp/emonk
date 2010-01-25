
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

inline void
write_int32(unsigned char* data, int val)
{
    data[0] = (val >> 24) & 0xFF;
    data[1] = (val >> 16) & 0xFF;
    data[2] = (val >> 8) & 0xFF;
    data[3] = val & 0xFF;
}

emonk_req_t*
read_req_info(uint cmd, unsigned char* buf, int length)
{
    emonk_req_t* ret = NULL;
    int idx = 0;

    ret = driver_alloc(sizeof(emonk_req_t));
    if(ret == NULL) goto error;
    ret->ok = 0;
    
    // Parse the enclosing structure.
    if(buf[idx++] != VERSION_MAGIC) goto error;
    if(buf[idx++] != SMALL_TUPLE) goto error;
    if(cmd == 0 && buf[idx] != 2) goto error;
    if(cmd == 1 && buf[idx] != 4) goto error;
    idx += 1;

    // Parse call token
    if(length-idx < 5) goto error;
    if(buf[idx++] != BINARY) goto error;
    ret->cid_len = read_int32(buf+idx, &idx);
    ret->call_id = buf + idx;
    idx += ret->cid_len;

    if(cmd == 0) // Reading a script
    {
        if(length-idx < 5) goto error;
        if(buf[idx++] != BINARY) goto error;
        ret->scr_len = read_int32(buf+idx, &idx);
        ret->script = buf + idx;
        idx += ret->scr_len;
        
        return ret;
    }
    else // Reading a function signature.
    {
        goto error;
    }
    
    
error:
    if(ret != NULL) driver_free(ret);
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