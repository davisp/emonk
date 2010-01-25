#ifndef EMONK_H
#define EMONK_H

#include <erl_driver.h>
#include <jsapi.h>

#include "emonk_comm.h"
#include "emonk_util.h"

typedef struct _emonk_vm_t
{
    JSRuntime* rt;
    JSContext* cx;
    JSObject* gl;
} emonk_vm_t;

emonk_vm_t* init_vm(emonk_settings_t* settings);
int stop_vm(emonk_vm_t* vm);
void* vm_eval(emonk_vm_t* vm, emonk_req_t* req, int* length);

#endif // Included emonk.h