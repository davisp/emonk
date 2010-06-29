#include "erl_nif.h"

#include "req.h"
#include "state.h"
#include "vm.h"

#define JS_RUNTIME_GC_THRESH 10485760 // 10 MiB
#define JS_GC_MAX_BYTES 8388608
#define JS_GC_MAX_MALLOC 8388608

state_t*
state_create(ErlNifEnv* env)
{
    ErlNifResourceType* res;
    state_t* state = enif_alloc(sizeof(state_t));
    const char* mod = "emonk";
    const char* name = "Context";
    int opts = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    if(state == NULL) goto error;
    state->lock = NULL;
    state->requests = NULL;
    state->workers = NULL;
    state->thread_opts = NULL;
    state->ctx_res = NULL;
    state->runtime = NULL;

    state->lock = enif_mutex_create("state_lock");
    if(state->lock == NULL) goto error;

    state->requests = queue_create(req_destroy);
    if(state->requests == NULL) goto error;

    state->deaths = queue_create(worker_destroy);

    state->thread_opts = enif_thread_opts_create("default_opts");
    if(state->thread_opts == NULL) goto error;

    state->workers = worker_create(state->thread_opts, state->requests,
                                                            state->deaths);
    if(state->workers == NULL) goto error;

    state->workers->next = worker_create(state->thread_opts, state->requests,
                                                            state->deaths);
    if(state->workers->next == NULL) goto error;
    state->workers->next->prev = state->workers;

    res = enif_open_resource_type(env, NULL, name, vm_destroy, opts, NULL);
    if(res == NULL) goto error;
    state->ctx_res = res;

    state->atom_ok = enif_make_atom(env, "ok");
    state->atom_error = enif_make_atom(env, "error");

    state->runtime = JS_NewRuntime(JS_RUNTIME_GC_THRESH);
    if(state->runtime == NULL) goto error;
    JS_SetGCParameter(state->runtime, JSGC_MAX_BYTES, JS_GC_MAX_BYTES);
    JS_SetGCParameter(state->runtime, JSGC_MAX_MALLOC_BYTES, JS_GC_MAX_MALLOC);
    
    goto success;

error:
    state_destroy(state);
    state = NULL;
success:
    return state;
}

void
state_destroy(state_t* state)
{
    worker_t* worker;

    if(state == NULL)
    {
        return;
    }

    if(state->runtime != NULL)
    {
        JS_DestroyRuntime(state->runtime);
    }

    while(state->workers != NULL)
    {
        worker = state->workers->next;
        worker_destroy(state->workers);
        state->workers = worker;
    }

    if(state->thread_opts != NULL)
    {
        enif_thread_opts_destroy(state->thread_opts);
    }
    
    if(state->deaths != NULL)
    {
        queue_destroy(state->deaths);
    }

    if(state->requests != NULL)
    {
        queue_destroy(state->requests);
    }

    if(state->lock != NULL)
    {
        enif_mutex_destroy(state->lock);
    }

    if(state != NULL)
    {
        enif_free(state);
    }
}

int
state_add_worker(state_t* state)
{
    worker_t* worker;
    int ret;

    enif_mutex_lock(state->lock);

    worker = state->workers;
    while(worker->next != NULL)
    {
        worker = worker->next;
    }

    worker->next = worker_create(state->thread_opts, state->requests,
                                                            state->deaths);
    if(worker->next != NULL)
    {
        worker->next->prev = worker;
        ret = 1;
    }
    else
    {
        ret = 0;
    }

    enif_mutex_unlock(state->lock);

    return ret;
}

int
state_rem_worker(state_t* state)
{
    req_t* req = req_create(req_close, NULL);
    worker_t* worker;

    if(req == NULL) return 0;

    enif_mutex_lock(state->lock);

    if(state->workers->next == NULL)
    {
        enif_mutex_unlock(state->lock);
        return 0;
    }

    queue_sneak(state->requests, req);

    worker = (worker_t*) queue_pop(state->deaths);
    if(worker == state->workers)
    {
        state->workers = worker->next;
    }
    queue_done(state->deaths, worker);

    enif_mutex_unlock(state->lock);

    return 1;
}

int
state_num_workers(state_t* state)
{
    worker_t* worker;
    int ret;

    enif_mutex_lock(state->lock);

    ret = 0;
    worker = state->workers;
    while(worker != NULL)
    {
        ret += 1;
        worker = worker->next;
    }

    enif_mutex_unlock(state->lock);

    return ret;
}

