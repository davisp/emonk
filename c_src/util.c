
#include <string.h>

#include "vm.h"
#include "util.h"

void
report_error(JSContext* cx, const char* mesg, JSErrorReport* report)
{
    void* job;
    ErlNifBinary bmesg;
    ErlNifBinary bsrc;

    job = JS_GetContextPrivate(cx);
    if(job == NULL) return;

    if(!(report->flags & JSREPORT_EXCEPTION)) return;

    if(mesg == NULL) mesg = "";
    if(report->linebuf == NULL) report->linebuf = "";

    if(!enif_alloc_binary(strlen(mesg), &bmesg)) return;
    if(!enif_alloc_binary(strlen(report->linebuf), &bsrc)) return;

    memcpy(bmesg.data, mesg, strlen(mesg));
    memcpy(bsrc.data, report->linebuf, strlen(report->linebuf));

    vm_set_error(job, bmesg, bsrc, report->lineno);
}

ENTERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ENTERM ret;
    
    if(enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) return ret;

    return enif_make_atom(env, atom);
}

ENTERM
mk_ok(ErlNifEnv* env, ENTERM value)
{
    ENTERM ok = mk_atom(env, "ok");
    return enif_make_tuple2(env, ok, value);
}

ENTERM
mk_error(ErlNifEnv* env, ENTERM value)
{
    ENTERM error = mk_atom(env, "error");
    return enif_make_tuple2(env, error, value);
}

ENTERM
emonk_no_memory(ErlNifEnv* env)
{
    return mk_error(env, mk_atom(env, "no_memory"));
}

ENTERM
emonk_init_failed(ErlNifEnv* env)
{
    return mk_error(env, mk_atom(env, "init_failed"));
}

uint32
cfg_uint(ErlNifEnv* env, ENTERM props, const char* atom, uint32 def)
{
    char key[512];
    ENTERM head;
    ENTERM tail;
    const ENTERM *tuple;
    int arity;
    uint32 ret;
    
    if(!enif_get_list_cell(env, props, &head, &tail))
    {
        return def;
    }
    
    do {
        if(!enif_get_tuple(env, head, &arity, &tuple)) continue;
        if(!enif_get_atom(env, tuple[0], key, 512, ERL_NIF_LATIN1)) continue;
        if(strcmp(key, atom) != 0) continue;
        if(!enif_get_uint(env, tuple[1], &ret)) return MAX_SETTING_VALUE + 1;
        return ret;
    } while(enif_get_list_cell(env, tail, &head, &tail));

    return def;
}

void
debug_jsval(JSContext* cx, jsval val)
{
    JSString* str;
    char* bytes;
    
    str = JS_ValueToString(cx, val);
    if(!str)
    {
        fprintf(stderr, "DEBUG: Unable to convert value.\n");
        return;
    }
    
    bytes = JS_EncodeString(cx, str);
    if(!bytes)
    {
        fprintf(stderr, "DEBUG: Unable to encode string.\n");
        return;
    }
    
    fprintf(stderr, "%s\n", bytes);
}
