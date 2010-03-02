#ifndef EMONK_H
#define EMONK_H

#include <erl_nif.h>
#include <jsapi.h>

#define RT_MAX_BYTES 1048576
#define GC_MAX_BYTES 8388608
#define GC_MAX_MALLOC 8388608
#define CONTEXT_STACK 8192

#define BEGIN_REQ(cx)           \
    JS_SetContextThread(cx);    \
    JS_BeginRequest(cx);

#define END_REQ(cx)             \
    JS_EndRequest(cx);          \
    JS_ClearContextThread(cx);

typedef struct _emonk_vm_t
{
    ErlNifEnv* env;
    ERL_NIF_TERM error;

    JSRuntime* rt;
    JSContext* cx;
    JSObject* gl;
} emonk_vm_t;

int init_vm(emonk_vm_t* vm, ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
void stop_vm(ErlNifEnv* env, void* obj);
// void* vm_eval(emonk_vm_t* vm, emonk_req_t* req, int* length);
// void* vm_call(emonk_vm_t* vm, emonk_req_t* req, int* length);

#endif // Included emonk.h