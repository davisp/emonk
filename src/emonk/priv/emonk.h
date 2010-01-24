#ifndef EMONK_H
#define EMONK_H

#include <erl_driver.h>
#include <jsapi.h>

typedef struct _emonk_vm_t
{
    JSRuntime* rt;
    JSContext* cx;
    JSObject* gl;
} emonk_vm_t;

emonk_vm_t* init_vm(uint rt_max, uint gc_max, uint gc_last, uint ctx);
int stop_vm(emonk_vm_t* vm);
void* vm_eval(emonk_vm_t* vm, const char *code, int clen, int* rlen);

#endif // Included emonk.h