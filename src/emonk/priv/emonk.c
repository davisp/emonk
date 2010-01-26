
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

emonk_vm_t*
init_vm(emonk_settings_t* settings)
{
    uint32 flags = 0;
    emonk_vm_t* vm = driver_alloc(sizeof(emonk_vm_t));

    if(vm == NULL) return NULL;
    memset(vm, 0, sizeof(emonk_vm_t));

    vm->rt = JS_NewRuntime(settings->rt_max_bytes);
    if(vm->rt == NULL) goto error;
    
    JS_SetGCParameter(vm->rt, JSGC_MAX_BYTES, settings->gc_max_bytes);
    JS_SetGCParameter(vm->rt, JSGC_MAX_MALLOC_BYTES, settings->gc_max_malloc);
    
    vm->cx = JS_NewContext(vm->rt, settings->context_stack);
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
    JS_SetErrorReporter(vm->cx, mk_error);
    
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
vm_eval(emonk_vm_t* vm, emonk_req_t* req, int* length)
{
    ErlDrvBinary* error;
    void* ret = NULL;
    jsval rval;
    int i, cnt;
    
    for(i = 0, cnt = 0; i < req->scr_len; i++)
    {
        if(req->script[i] == '\n') cnt += 1;
    }

    BEGIN_REQ(vm->cx);
    JS_SetContextPrivate(vm->cx, NULL);
    if(!JS_EvaluateScript(vm->cx, vm->gl, req->script, req->scr_len, "", cnt, &rval))
    {
        error = JS_GetContextPrivate(vm->cx);
        if(error == NULL)
        {
            ret = NULL;
        }
        else
        {
            ret = driver_alloc(error->orig_size);
            if(ret == NULL)
            {
                ret = NULL;
            }
            else
            {
                memcpy(ret, error->orig_bytes, error->orig_size);
                *length = error->orig_size;
            }
            driver_free_binary(error);
        }
    }
    else
    {
        req->ok = 1;
        ret = to_erl(vm->cx, rval, length);
    }

    JS_MaybeGC(vm->cx);
    END_REQ(vm->cx);
    return ret;
}

void*
vm_call(emonk_vm_t* vm, emonk_req_t* req, int* length)
{
    ErlDrvBinary* error;
    void* ret = NULL;
    jsval func, rval;
    int i, cnt;

    BEGIN_REQ(vm->cx);
    JS_SetContextPrivate(vm->cx, NULL);
        
    if(!JS_GetProperty(vm->cx, vm->gl, req->function, &func))
    {
        return NULL;
    }

    if(JS_TypeOfValue(vm->cx, func) != JSTYPE_FUNCTION)
    {
        return NULL;
    }

    if(!JS_CallFunctionValue(vm->cx, vm->gl, func, req->argc, req->argv, &rval))
    {
        error = JS_GetContextPrivate(vm->cx);
        if(error == NULL)
        {
            ret = NULL;
        }
        else
        {
            ret = driver_alloc(error->orig_size);
            if(ret == NULL)
            {
                ret = NULL;
            }
            else
            {
                memcpy(ret, error->orig_bytes, error->orig_size);
                *length = error->orig_size;
            }
            driver_free_binary(error);
        }
    }
    else
    {
        req->ok = 1;
        ret = to_erl(vm->cx, rval, length);
    }

    JS_MaybeGC(vm->cx);
    END_REQ(vm->cx);
    return ret;
}


