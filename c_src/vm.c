#include <assert.h>
#include <string.h>

#include "util.h"
#include "vm.h"

#define BEGIN_REQ(cx)           \
    JS_SetContextThread(cx);    \
    JS_BeginRequest(cx);

#define END_REQ(cx)             \
    JS_EndRequest(cx);          \
    JS_ClearContextThread(cx);

// A job is a representation of a function call for eval or
// call. It is created and placed into the VM queue to be
// processed when a worker becomes ready to run jobs for this
// VM.

typedef enum
{
    job_unknown,
    job_eval,
    job_call
} job_type_e;

struct job_t
{
    job_type_e      type;

    ErlNifEnv*      env;
    ENTERM          ref;
    ErlNifPid       pid;
    
    ErlNifBinary    script;
    ENTERM          name;
    ENTERM          args;

    ENTERM          error;
};

typedef struct job_t* job_ptr;

// A VM is the grouping of a JSContext pointer and a queue
// of jobs related to function calls. As workers handle VM's
// they'll run the job and then send the output to the PID
// that requested the job.

struct vm_t
{
    ErlNifMutex*    lock;
    
    vm_status_e     status;
    
    JSContext*      cx;
    JSObject*       gl;

    queue_ptr       jobs;
};

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

ENTERM vm_eval(vm_ptr vm, job_ptr job);
ENTERM vm_call(vm_ptr vm, job_ptr job);
void job_destroy(void* obj);

job_ptr
job_create()
{
    job_ptr ret = (job_ptr) enif_alloc(sizeof(struct job_t));
    if(ret == NULL) return NULL;

    ret->type = job_unknown;
    ret->env = enif_alloc_env();
    if(ret->env == NULL) goto error;

    ret->ref = 0;
    ret->script.data = NULL;
    ret->script.size = 0;
    ret->error = 0;

    return ret;

error:
    if(ret->env != NULL) enif_free_env(ret->env);
    enif_free(ret);
    return NULL;
}

void
job_destroy(void* obj)
{
    job_ptr job = (job_ptr) obj;
    if(job->script.data != NULL) enif_release_binary(&job->script);
    if(job->env != NULL) enif_free_env(job->env);
    enif_free(job);
}

//
//  VM Related API
//

vm_ptr
vm_init(state_ptr state, size_t stack_size)
{
    ErlNifResourceType* res_type = state_res_type(state);
    JSRuntime* rt = state_js_runtime(state);
    int flags;

    vm_ptr vm = (vm_ptr) enif_alloc_resource(res_type, sizeof(struct vm_t));
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

    vm->jobs = queue_create("context_jobs");
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

    return vm;

error:
    enif_release_resource(vm);
    return NULL;
}
 
void
vm_destroy(ErlNifEnv* env, void* obj)
{
    vm_ptr vm = (vm_ptr) obj;
    
    if(vm->cx != NULL)
    {
        JS_SetContextThread(vm->cx);
        JS_DestroyContext(vm->cx);
    }
    
    if(vm->jobs != NULL)
    {
        queue_destroy(vm->jobs);
    }
    
    if(vm->lock != NULL)
    {
        enif_mutex_destroy(vm->lock);
    }
}

vm_status_e
vm_set_status(vm_ptr vm, vm_status_e status)
{
    vm_status_e ret;
    
    enif_mutex_lock(vm->lock);

    ret = vm->status;

    if(vm->status == vm_running && status == vm_idle)
    {
        // An idle VM can be released.
        enif_release_resource(vm);
        vm->status = status;
    }
    else if(vm->status == vm_idle && status == vm_running)
    {
        // Keep a reference to the resource so
        // it isn't released out from under us.
        enif_keep_resource(vm);
        vm->status = status;
    }

    enif_mutex_unlock(vm->lock);

    return ret;
}

ENTERM
vm_add_eval(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    vm_ptr vm;
    job_ptr job;
    ErlNifBinary script;

    if(argc != 4) return enif_make_badarg(env);
    
    if(!enif_get_resource(env, argv[0], state_res_type(state), (void**) &vm))
    {
        return enif_make_badarg(env);
    }

    job = job_create();
    if(job == NULL) return enif_make_badarg(env);
    job->type = job_eval; 
    job->ref = enif_make_copy(job->env, argv[1]);
    if(!enif_get_local_pid(env, argv[2], &job->pid)) goto error; 
    if(!enif_inspect_binary(env, argv[3], &script)) goto error;
    if(!enif_alloc_binary(script.size, &(job->script))) goto error;
    memcpy(job->script.data, script.data, script.size);

    if(!queue_push(vm->jobs, job))
    {
        goto error;
    }
    job = NULL;

    if(vm_set_status(vm, vm_running) == vm_idle)
    {
        if(!queue_push(state_req_queue(state), vm))
        {
            goto error;
        }
    }

    return state_ok(state);

error:
    if(job != NULL) job_destroy(job);
    return state_error(state);
}

ENTERM
vm_add_call(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    vm_ptr vm;
    job_ptr job;

    if(argc != 5) return enif_make_badarg(env);
    
    if(!enif_get_resource(env, argv[0], state_res_type(state), (void**) &vm))
    {
        return enif_make_badarg(env);
    }

    job = job_create();
    if(job == NULL) return enif_make_badarg(env);
    job->type = job_call; 
    job->ref = enif_make_copy(job->env, argv[1]);
    if(!enif_get_local_pid(env, argv[2], &job->pid)) goto error;
    job->name = enif_make_copy(job->env, argv[3]);
    job->args = enif_make_copy(job->env, argv[4]);

    if(!queue_push(vm->jobs, job))
    {
        goto error;
    }
    job = NULL;

    if(vm_set_status(vm, vm_running) == vm_idle)
    {
        if(!queue_push(state_req_queue(state), vm))
        {
            goto error;
        }
    }

    return state_ok(state);

error:
    if(job != NULL) job_destroy(job);
    return state_error(state);
}

void
vm_run_job(vm_ptr vm, queue_ptr req_q)
{
    job_ptr job;
    ENTERM resp;

    job = queue_pop_nowait(vm->jobs);
    if(job == NULL)
    {
        vm_set_status(vm, vm_idle);
        return;
    }

    BEGIN_REQ(vm->cx);
    JS_SetContextPrivate(vm->cx, job);

    switch(job->type)
    {
        case job_eval:
            resp = vm_eval(vm, job);
            break;

        case job_call:
            resp = vm_call(vm, job);
            break;

        default:
            assert(0 && "Invalid job: Bad type.");
            return; // silence warning
    }

    JS_SetContextPrivate(vm->cx, job);
    JS_MaybeGC(vm->cx);
    END_REQ(vm->cx);

    // Should we just log this? Or ignore it?
    if(!enif_send(NULL, &(job->pid), job->env, resp))
    {
        assert(0 && "Failed to send response for job.");
    }

    job_destroy(job);

    if(!queue_has_item(vm->jobs))
    {
        vm_set_status(vm, vm_idle);
    }
    else
    {
        queue_push(req_q, vm);
    }

    return;
}

ENTERM
vm_eval(vm_ptr vm, job_ptr job)
{
    ENTERM resp;
    const char* script;
    size_t length;
    jsval rval;
    int cnt;
    int i;

    script = (const char*) job->script.data;
    length = job->script.size;

    for(i = 0, cnt = 0; i < length; i++)
    {
        if(script[i] == '\n') cnt += 1;
    }

    if(!JS_EvaluateScript(vm->cx, vm->gl, script, length, "", cnt, &rval))
    {
        if(job->error != 0)
        {
            resp = mk_error(job->env, job->error);
        }
        else
        {
            resp = mk_error(job->env, mk_atom(job->env, "unknown_error"));
        }
    }
    else
    {
        resp = mk_ok(job->env, to_erl(job->env, vm->cx, rval));
    }

    return enif_make_tuple2(job->env, job->ref, resp);
}

ENTERM
vm_call(vm_ptr vm, job_ptr job)
{
    ENTERM resp;
    ENTERM head;
    ENTERM tail;
    jsval func;
    jsval args[256];
    jsval rval;
    jsid idp;
    int argc;
    
    // Get the function object.
    
    func = to_js(job->env, vm->cx, job->name);
    if(func == JSVAL_VOID)
    {
        resp = mk_error(job->env, mk_atom(job->env, "invalid_name"));
        goto send;
    }

    if(!JS_ValueToId(vm->cx, func, &idp))
    {
        resp = mk_error(job->env, mk_atom(job->env, "internal_error"));
        goto send;
    }
    
    if(!JS_GetPropertyById(vm->cx, vm->gl, idp, &func))
    {
        resp = mk_error(job->env, mk_atom(job->env, "bad_property"));
        goto send;
    }

    if(JS_TypeOfValue(vm->cx, func) != JSTYPE_FUNCTION)
    {
        resp = mk_error(job->env, mk_atom(job->env, "not_a_function"));
        goto send;
    }

    // Creating function arguments.
    
    if(enif_is_empty_list(job->env, job->args))
    {
        argc = 0;
    }
    else
    {
        if(!enif_get_list_cell(job->env, job->args, &head, &tail))
        {
            resp = mk_error(job->env, mk_atom(job->env, "invalid_argv"));
            goto send;
        }

        argc = 0;
        do {
            args[argc++] = to_js(job->env, vm->cx, head);
        } while(enif_get_list_cell(job->env, tail, &head, &tail) && argc < 256);
    }

    // Call function
    if(!JS_CallFunctionValue(vm->cx, vm->gl, func, argc, args, &rval))
    {
        if(job->error != 0)
        {
            resp = mk_error(job->env, job->error);
        }
        else
        {
            resp = mk_error(job->env, mk_atom(job->env, "unknown_error"));
        }
    }
    else
    {
        resp = mk_ok(job->env, to_erl(job->env, vm->cx, rval));
    }

send:
    return enif_make_tuple2(job->env, job->ref, resp);
}

void
vm_set_error(void* obj, ENBINARY mesg, ENBINARY src, unsigned int line)
{
    job_ptr job = (job_ptr) obj;

    ENTERM tmesg = enif_make_binary(job->env, &mesg);
    ENTERM tsrc = enif_make_binary(job->env, &src);
    ENTERM tline = enif_make_int(job->env, line);

    job->error = enif_make_tuple3(job->env, tmesg, tsrc, tline);
}
