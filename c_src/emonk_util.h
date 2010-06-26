
#ifndef EMONK_UTIL_H
#define EMONK_UTIL_H

#include <erl_nif.h>
#include <js/jsapi.h>

#define MAX_SETTING_VALUE 2147483648u

void report_error(JSContext* cx, const char* mesg, JSErrorReport* report);
jsval to_js(ErlNifEnv* env, JSContext* cx, ERL_NIF_TERM term);
ERL_NIF_TERM to_erl(ErlNifEnv* env, JSContext* cx, jsval val);

ERL_NIF_TERM emonk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM emonk_ok(ErlNifEnv* env, ERL_NIF_TERM value);
ERL_NIF_TERM emonk_error(ErlNifEnv* env, ERL_NIF_TERM value);
ERL_NIF_TERM emonk_no_memory(ErlNifEnv* env);
ERL_NIF_TERM emonk_init_failed(ErlNifEnv* env);

uint32 cfg_uint(ErlNifEnv* env, ERL_NIF_TERM pl, const char* atom, uint32 def);


void debug_jsval(JSContext* cx, jsval val);

#endif // Included util.h
