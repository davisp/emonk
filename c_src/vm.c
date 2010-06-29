#include "util.h"
#include "vm.h"

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

int
vm_init(vm_t* vm, JSRuntime* rt, size_t stack_size)
{
    int flags;

    if(vm == NULL) return 0;

    vm->lock = NULL;
    vm->status = vm_idle;
    vm->cx = NULL;
    vm->gl = NULL;
    vm->jobs = NULL;

    vm->lock = enif_mutex_create("vm_lock");
    if(vm->lock == NULL) goto error;

    vm->cx = JS_NewContext(rt, stack_size);
    if(vm->cx == NULL) goto error;

    vm->jobs = queue_create(job_destroy);
    if(vm->jobs == NULL) goto error;

    BEGIN_REQ(vm->cx);

    flags = 0;
    flags |= JSOPTION_VAROBJFIX;
    flags |= JSOPTION_STRICT;
    flags |= JSVERSION_LATEST;
    flags |= JSOPTION_COMPILE_N_GO;
    flags |= JSOPTION_XML;
    JS_SetOptions(vm->cx, JS_GetOptions(vm->cx) | flags);
    
    vm->gl = JS_NewObject(vm->cx, &global_class, NULL, NULL);
    if(vm->gl == NULL) goto error;
    if(!JS_InitStandardClasses(vm->cx, vm->gl)) goto error;
    JS_SetErrorReporter(vm->cx, report_error);
    
    END_REQ(vm->cx);

    return 1;

error:
    queue_destroy(vm->jobs);
    JS_DestroyContext(vm->cx);
    enif_mutex_destroy(vm->lock);
    return 0;
}
 
void
vm_destroy(ErlNifEnv* env, void* obj)
{
    vm_t* vm = (vm_t*) obj;
    
    JS_SetContextThread(vm->cx);
    JS_DestroyContext(vm->cx);
    enif_mutex_destroy(vm->lock);
    queue_destroy(vm->jobs);
}


