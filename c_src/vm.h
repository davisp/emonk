#ifndef EMONK_VM_H
#define EMONK_VM_H

#include <js/jsapi.h>
#include "erl_nif.h"

#include "alias.h"
#include "job.h"
#include "queue.h"

#define BEGIN_REQ(cx)           \
    JS_SetContextThread(cx);    \
    JS_BeginRequest(cx);

#define END_REQ(cx)             \
    JS_EndRequest(cx);          \
    JS_ClearContextThread(cx);

typedef enum
{
    vm_idle,
    vm_running
} vm_status_e;

typedef struct
{
    ErlNifMutex*    lock;
    vm_status_e     status;

    JSContext*      cx;
    JSObject*       gl;

    queue_t*        jobs;
} vm_t;

int vm_init(vm_t* vm, JSRuntime* rt, size_t stack_size);
void vm_destroy(ErlNifEnv* env, void* obj);


#endif // Included vm.h
