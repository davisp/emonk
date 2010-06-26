
#include <string.h>

#include "emonk.h"
#include "emonk_util.h"

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
init_vm(emonk_vm_t* vm, ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32 flags = 0;
    uint32 rt_max_bytes = RT_MAX_BYTES;
    uint32 gc_max_bytes = GC_MAX_BYTES;
    uint32 gc_max_malloc = GC_MAX_MALLOC;
    uint32 ctx_stack = CONTEXT_STACK;
    
    if(vm == NULL) return 0;
    memset(vm, 0, sizeof(emonk_vm_t));

    if(argc > 0)
    {
        rt_max_bytes = cfg_uint(env, argv[0], "rt_max_bytes", RT_MAX_BYTES);
        gc_max_bytes = cfg_uint(env, argv[0], "gc_max_bytes", GC_MAX_BYTES);
        gc_max_malloc = cfg_uint(env, argv[0], "gc_max_malloc", GC_MAX_MALLOC);
        ctx_stack = cfg_uint(env, argv[0], "ctx_stack", CONTEXT_STACK);
        
        if(rt_max_bytes > MAX_SETTING_VALUE) return 0;
        if(gc_max_bytes > MAX_SETTING_VALUE) return 0;
        if(gc_max_malloc > MAX_SETTING_VALUE) return 0;
        if(ctx_stack > MAX_SETTING_VALUE) return 0;        
    }
    
    vm->rt = JS_NewRuntime(rt_max_bytes);
    if(vm->rt == NULL) goto error;

    JS_SetGCParameter(vm->rt, JSGC_MAX_BYTES, gc_max_bytes);
    JS_SetGCParameter(vm->rt, JSGC_MAX_MALLOC_BYTES, gc_max_malloc);
    
    vm->cx = JS_NewContext(vm->rt, ctx_stack);
    if(vm->cx == NULL) goto error;
    JS_SetContextPrivate(vm->cx, vm);

    BEGIN_REQ(vm->cx);

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
    if(vm->cx != NULL) JS_DestroyContext(vm->cx);
    if(vm->rt != NULL) JS_DestroyRuntime(vm->rt);
    return 0;
}

void
stop_vm(ErlNifEnv* env, void* obj)
{
    emonk_vm_t* vm = (emonk_vm_t*) obj;
    JS_SetContextThread(vm->cx);
    JS_DestroyContext(vm->cx);
    JS_DestroyRuntime(vm->rt);
}

ERL_NIF_TERM
vm_eval(ErlNifEnv* env, emonk_vm_t* vm, const char* script, unsigned int length)
{
    ERL_NIF_TERM ret;
    jsval rval;
    int cnt;
    int i;
    
    for(i = 0, cnt = 0; i < length; i++)
    {
        if(script[i] == '\n') cnt += 1;
    }

    BEGIN_REQ(vm->cx);

    if(!JS_EvaluateScript(vm->cx, vm->gl, script, length, "", cnt, &rval))
    {
        if(vm->error != 0)
        {
            ret = emonk_error(env, vm->error);
        }
        else
        {
            ret = emonk_error(env, emonk_atom(env, "unknown_error"));
        }
    }
    else
    {
        ret = emonk_ok(env, to_erl(env, vm->cx, rval));
    }
    JS_MaybeGC(vm->cx);

    END_REQ(vm->cx);

    return ret;
}

ERL_NIF_TERM
vm_call(ErlNifEnv* env, emonk_vm_t* vm, ERL_NIF_TERM name, ERL_NIF_TERM argv)
{
    ERL_NIF_TERM ret;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    jsval func;
    jsval args[256];
    jsval rval;
    jsid idp;
    int argc;

    BEGIN_REQ(vm->cx);
    
    // Getting function object.
    
    func = to_js(env, vm->cx, name);
    if(func == JSVAL_VOID)
    {
        return emonk_error(env, emonk_atom(env, "invalid_function_name"));
    }

    if(!JS_ValueToId(vm->cx, func, &idp))
    {
        return emonk_error(env, emonk_atom(env, "internal_error"));
    }
    
    if(!JS_GetPropertyById(vm->cx, vm->gl, idp, &func))
    {
        return emonk_error(env, emonk_atom(env, "bad_property"));
    }

    if(JS_TypeOfValue(vm->cx, func) != JSTYPE_FUNCTION)
    {
        return emonk_error(env, emonk_atom(env, "not_a_function"));
    }

    // Creating function arguments.
    
    if(enif_is_empty_list(env, argv))
    {
        argc = 0;
    }
    else
    {
        if(!enif_get_list_cell(env, argv, &head, &tail))
        {
            return emonk_error(env, emonk_atom(env, "invalid_argv"));
        }

        argc = 0;
        do {
            args[argc++] = to_js(env, vm->cx, head);
        } while(enif_get_list_cell(env, tail, &head, &tail) && argc < 256);
    }

    // Call function
    if(!JS_CallFunctionValue(vm->cx, vm->gl, func, argc, args, &rval))
    {
        if(vm->error != 0)
        {
            ret = emonk_error(env, vm->error);
        }
        else
        {
            ret = emonk_error(env, emonk_atom(env, "unknown_error"));
        }
    }
    else
    {
        ret = emonk_ok(env, to_erl(env, vm->cx, rval));
    }

    JS_MaybeGC(vm->cx);
    END_REQ(vm->cx);
    return ret;
}
