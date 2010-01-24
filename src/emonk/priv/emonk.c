
#include <string.h>

#include "emonk.h"
#include "emonk_comm.h"

#define BEGIN_REQ(cx)           \
    JS_SetContextThread(cx);    \
    JS_BeginRequest(cx);

#define END_REQ(cx)             \
    JS_EndRequest(cx);          \
    JS_ClearContextThread(cx);

static JSClass global_class = {
    "global",
    JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

void on_error(JSContext* cx, const char* mesg, JSErrorReport* report);
void* vm_error(emonk_vm_t* vm);
void* vm_response(emonk_vm_t* vm, jsval rval);

emonk_vm_t*
init_vm(uint rt_max, uint gc_max, uint gc_last, uint ctx)
{
    uint32 flags = 0;
    emonk_vm_t* vm = driver_alloc(sizeof(emonk_vm_t));

    if(vm == NULL) return NULL;
    memset(vm, 0, sizeof(emonk_vm_t));

    vm->rt = JS_NewRuntime(rt_max);
    if(vm->rt == NULL) goto error;
    
    JS_SetGCParameter(vm->rt, JSGC_MAX_BYTES, gc_max);
    JS_SetGCParameter(vm->rt, JSGC_MAX_MALLOC_BYTES, gc_last);
    
    vm->cx = JS_NewContext(vm->rt, ctx);
    if(vm->cx == NULL) goto error;
    BEGIN_REQ(vm->cx);

    flags |= JSOPTION_VAROBJFIX;
    flags |= JSOPTION_STRICT;
    flags |= JSVERSION_LATEST;
    flags |= JSOPTION_XML;
    JS_SetOptions(vm->cx, JS_GetOptions(vm->cx) | flags);
    
    vm->gl = JS_NewObject(vm->cx, &global_class, NULL, NULL);
    if(vm->gl == NULL) goto error;
    if(!JS_InitStandardClasses(vm->cx, vm->gl)) goto error;
    JS_SetErrorReporter(vm->cx, on_error);
    
    END_REQ(vm->cx);
    return vm;

error:
    if(vm->cx != NULL) JS_DestroyContext(vm->cx);
    if(vm->rt != NULL) JS_DestroyRuntime(vm->rt);
    driver_free(vm);
    return NULL;
}

int
stop_vm(emonk_vm_t* vm)
{
    JS_SetContextThread(vm->cx);
    JS_DestroyContext(vm->cx);
    JS_DestroyRuntime(vm->rt);
    driver_free(vm);
}

void*
vm_eval(emonk_vm_t* vm, const char *code, int clen, int* rlen)
{
    void* ret = NULL;
    jsval rval;

    BEGIN_REQ(vm->cx);
    JS_SetContextPrivate(vm->cx, NULL);
    if(!JS_EvaluateScript(vm->cx, vm->gl, code, clen, "", 1, &rval))
    {
        ret = vm_error(vm);
    }
    else
    {
        ret = to_erl(vm->cx, rval, rlen);
    }

    JS_MaybeGC(vm->cx);

    END_REQ(vm->cx);
    return ret;
}

void*
vm_error(emonk_vm_t* vm)
{
    return NULL;
}

void
on_error(JSContext* cx, const char* mesg, JSErrorReport* report)
{
    return;
}

