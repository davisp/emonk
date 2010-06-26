
#include <string.h>

#include "emonk.h"
#include "emonk_util.h"

void
report_error(JSContext* cx, const char* mesg, JSErrorReport* report)
{
    emonk_vm_t* vm = NULL;

    ErlNifBinary bmesg;
    ErlNifBinary bsrc;
    
    ERL_NIF_TERM tmesg;
    ERL_NIF_TERM tsrc;
    ERL_NIF_TERM tline;

    vm = (emonk_vm_t*) JS_GetContextPrivate(cx);
    if(vm == NULL) return;

    if(!(report->flags & JSREPORT_EXCEPTION)) return;

    if(mesg == NULL) mesg = "";
    if(report->linebuf == NULL) report->linebuf = "";

    if(!enif_alloc_binary(strlen(mesg), &bmesg)) return;
    if(!enif_alloc_binary(strlen(report->linebuf), &bsrc)) return;

    memcpy(bmesg.data, mesg, strlen(mesg));
    memcpy(bsrc.data, report->linebuf, strlen(report->linebuf));

    tmesg = enif_make_binary(vm->env, &bmesg);
    tsrc = enif_make_binary(vm->env, &bsrc);
    tline = enif_make_int(vm->env, report->lineno);
    
    vm->error = enif_make_tuple3(vm->env, tmesg, tsrc, tline);
}

ERL_NIF_TERM
emonk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;
    
    if(enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) return ret;

    return enif_make_atom(env, atom);
}

ERL_NIF_TERM
emonk_ok(ErlNifEnv* env, ERL_NIF_TERM value)
{
    ERL_NIF_TERM ok = emonk_atom(env, "ok");
    return enif_make_tuple2(env, ok, value);
}

ERL_NIF_TERM
emonk_error(ErlNifEnv* env, ERL_NIF_TERM value)
{
    ERL_NIF_TERM error = emonk_atom(env, "error");
    return enif_make_tuple2(env, error, value);
}

ERL_NIF_TERM
emonk_no_memory(ErlNifEnv* env)
{
    return emonk_error(env, emonk_atom(env, "no_memory"));
}

ERL_NIF_TERM
emonk_init_failed(ErlNifEnv* env)
{
    return emonk_error(env, emonk_atom(env, "init_failed"));
}

uint32
cfg_uint(ErlNifEnv* env, ERL_NIF_TERM pl, const char* atom, uint32 def)
{
    char key[512];
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    const ERL_NIF_TERM *tuple;
    int arity;
    uint32 ret;
    
    if(!enif_get_list_cell(env, pl, &head, &tail))
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
