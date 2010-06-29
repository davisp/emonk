#include <time.h>

#include "util.h"
#include "worker.h"

void* worker_run(void* arg);
void worker_exec(vm_t* vm);
void worker_eval(vm_t* vm, job_t* job);
void worker_call(vm_t* vm, job_t* job);

worker_t*
worker_create(ErlNifThreadOpts* opts, queue_t* reqs, queue_t* deaths)
{
    worker_t* worker = enif_alloc(sizeof(worker_t));
    if(worker == NULL) return NULL;

    worker->lock = enif_mutex_create("worker_lock");
    if(worker->lock == NULL)
    {
        enif_free(worker);
        return NULL;
    }

    worker->status = worker_init;
    worker->requests = reqs;
    worker->deaths = deaths;
    worker->next = NULL;
    worker->prev = NULL;

    if(enif_thread_create("", &worker->tid, worker_run, worker, opts) != 0)
    {
        enif_free(worker);
        worker = NULL;
    }

    return worker;
}

void
worker_destroy(void* obj)
{
    worker_t* worker = (worker_t*) obj;
    enif_mutex_lock(worker->lock);
    worker->status = worker_terminated;
    void* resp;

    if(worker->prev != NULL)
    {
        worker->prev->next = worker->next;
    }

    if(worker->next != NULL)
    {
        worker->next->prev = worker->prev;
    }

    enif_thread_join(worker->tid, &resp);

    enif_mutex_unlock(worker->lock);
    enif_mutex_destroy(worker->lock);
    enif_free(worker);

    return;
}

void*
worker_run(void* arg)
{
    worker_t* self = (worker_t*) arg;
    queue_t* requests = self->requests;
    queue_t* deaths = self->deaths;
    req_t* req;

    enif_mutex_lock(self->lock);
    self->status = worker_running;
    enif_mutex_unlock(self->lock);

    while(1)
    {
        req = (req_t*) queue_pop(requests);
        
        switch(req->type)
        {
            case req_exec:
                worker_exec(req->vm);
                queue_done(requests, req);
                break;

            case req_close:
            default:
                queue_done(requests, req);
                queue_push(deaths, self);
                return NULL;
        }
    }
}

void
worker_exec(vm_t* vm)
{
    job_t* job;

    enif_mutex_lock(vm->lock);
    vm->status = vm_running;
    enif_mutex_unlock(vm->lock);

    job = queue_pop_nowait(vm->jobs);
    if(job == NULL) return;

    switch(job->type)
    {
        case job_eval:
            worker_eval(vm, job);
            break;

        case job_call:
            worker_call(vm, job);
            break;

        default:
            break;
    }
            
    queue_done(vm->jobs, job);

    enif_mutex_lock(vm->lock);
    vm->status = vm_idle;
    enif_mutex_unlock(vm->lock);

    return;
}

void
worker_eval(vm_t* vm, job_t* job)
{
    ERL_NIF_TERM resp;
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

    BEGIN_REQ(vm->cx);

    JS_SetContextPrivate(vm->cx, job);

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

    JS_SetContextPrivate(vm->cx, job);
    JS_MaybeGC(vm->cx);
    END_REQ(vm->cx);

    resp = enif_make_tuple2(job->env, job->ref, resp);
    enif_send(NULL, &(job->pid), job->env, resp);
}

void
worker_call(vm_t* vm, job_t* job)
{
    ENTERM resp;
    ENTERM head;
    ENTERM tail;
    jsval func;
    jsval args[256];
    jsval rval;
    jsid idp;
    int argc;

    BEGIN_REQ(vm->cx);
    
    JS_SetContextPrivate(vm->cx, job);

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

    JS_SetContextPrivate(vm->cx, job);
    JS_MaybeGC(vm->cx);
    END_REQ(vm->cx);

    resp = enif_make_tuple2(job->env, job->ref, resp);
    enif_send(NULL, &(job->pid), job->env, resp);
}
