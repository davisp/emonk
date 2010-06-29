
#ifndef EMONK_UTIL_H
#define EMONK_UTIL_H

#include <erl_nif.h>
#include <js/jsapi.h>

#include "alias.h"

#define MAX_SETTING_VALUE 2147483648u

void report_error(JSContext* cx, const char* mesg, JSErrorReport* report);
jsval to_js(ErlNifEnv* env, JSContext* cx, ENTERM term);
ENTERM to_erl(ErlNifEnv* env, JSContext* cx, jsval val);

ENTERM mk_atom(ErlNifEnv* env, const char* atom);
ENTERM mk_ok(ErlNifEnv* env, ENTERM value);
ENTERM mk_error(ErlNifEnv* env, ENTERM value);
ENTERM emonk_no_memory(ErlNifEnv* env);
ENTERM emonk_init_failed(ErlNifEnv* env);

uint32 cfg_uint(ErlNifEnv* env, ENTERM props, const char* atom, uint32 def);

void debug_jsval(JSContext* cx, jsval val);

#endif // Included util.h
