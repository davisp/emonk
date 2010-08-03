#include <assert.h>
#include <string.h>

#include "erl_nif.h"

#include "state.h"
#include "vm.h"
#include "worker.h"

#define GC_THRESHOLD 10485760 // 10 MiB
#define MAX_BYTES 8388608
#define MAX_MALLOC_BYTES 8388608
#define MAX_WORKERS 64

struct state_t
{
    ErlNifMutex*            lock;
    
    ErlNifResourceType*     ctx_res;
    
    queue_ptr               req_q;
    queue_ptr               dth_q;

    worker_ptr              workers[MAX_WORKERS];
    unsigned int            num_workers;
    ErlNifThreadOpts*       topts;
    
    JSRuntime*              runtime;

    ENTERM                  atom_ok;
    ENTERM                  atom_error;
};

state_ptr
state_create(ErlNifEnv* env)
{
    ErlNifResourceType* res;
    state_ptr state = (state_ptr) enif_alloc(sizeof(struct state_t));
    const char* name = "Context";
    int opts = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    if(state == NULL) goto error;
    state->lock = NULL;
    state->req_q = NULL;
    state->dth_q = NULL;
    state->num_workers = 0;
    state->topts = NULL;
    state->ctx_res = NULL;
    state->runtime = NULL;

    state->lock = enif_mutex_create("state_lock");
    if(state->lock == NULL) goto error;

    state->req_q = queue_create("requests");
    if(state->req_q == NULL) goto error;

    state->dth_q = queue_create("deaths");
    if(state->dth_q == NULL) goto error;

    state->topts = enif_thread_opts_create("default_opts");
    if(state->topts == NULL) goto error;

    res = enif_open_resource_type(env, NULL, name, vm_destroy, opts, NULL);
    if(res == NULL) goto error;
    state->ctx_res = res;

    state->atom_ok = enif_make_atom(env, "ok");
    state->atom_error = enif_make_atom(env, "error");

    state->runtime = JS_NewRuntime(GC_THRESHOLD);
    if(state->runtime == NULL) goto error;
    JS_SetGCParameter(state->runtime, JSGC_MAX_BYTES, MAX_BYTES);
    JS_SetGCParameter(state->runtime, JSGC_MAX_MALLOC_BYTES, MAX_MALLOC_BYTES);
    
    memset(state->workers, 0, MAX_WORKERS * sizeof(worker_ptr));
    if(!state_add_worker(state)) goto error;
    
    return state;

error:
    state_destroy(state);
    return NULL;
}

void
state_destroy(state_ptr state)
{
    int w;

    if(state == NULL)
    {
        return;
    }

    for(w = 0; w < state->num_workers; w++)
    {
        worker_destroy(state->workers[w]);
        state->workers[w] = NULL;
    }

    if(state->topts != NULL)
    {
        enif_thread_opts_destroy(state->topts);
    }
    
    if(state->dth_q != NULL)
    {
        queue_destroy(state->dth_q);
    }

    if(state->req_q != NULL)
    {
        queue_destroy(state->req_q);
    }

    // XXX: Big assumption that we can't call this destructor with
    //      contexts still alive. I should really add a linked list
    //      of vm's so that I can clear those out to avoid nasty
    //      errors.
    if(state->runtime != NULL)
    {
        JS_DestroyRuntime(state->runtime);
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

queue_ptr
state_req_queue(state_ptr state)
{
    assert(state->req_q != NULL && "Invalid state: No request queue.");
    return state->req_q;
}

ErlNifResourceType*
state_res_type(state_ptr state)
{
    ErlNifResourceType* ret;
    
    enif_mutex_lock(state->lock);
    ret = state->ctx_res;
    enif_mutex_unlock(state->lock);
    
    assert(ret != NULL && "Invalid state: No ctx_res!");
    
    return ret;
}

JSRuntime*
state_js_runtime(state_ptr state)
{
    JSRuntime* ret;
    
    enif_mutex_lock(state->lock);
    ret = state->runtime;
    enif_mutex_unlock(state->lock);
    
    assert(ret != NULL && "Invalid state: No JSRuntime");
    
    return ret;
}

ENTERM
state_ok(state_ptr state)
{
    return state->atom_ok;
}

ENTERM
state_error(state_ptr state)
{
    return state->atom_error;
}

unsigned int
state_num_workers(state_ptr state)
{
    unsigned int ret;

    enif_mutex_lock(state->lock);
    ret = state->num_workers;
    enif_mutex_unlock(state->lock);
    
    assert(ret <= MAX_WORKERS && "Invalid state: Too many workers.");
    
    return ret;
}

int
state_add_worker(state_ptr state)
{
    int w, ret;
    ret = 0;

    enif_mutex_lock(state->lock);
    
    // If we're at max workers, bail out.
    assert(state->num_workers <= MAX_WORKERS && "Invalid state: Too many workers.");
    if(state->num_workers == MAX_WORKERS)
    {
        enif_mutex_unlock(state->lock);
        return 0;
    }

    w = state->num_workers;

    state->workers[w] = worker_create(state->topts, state->req_q, state->dth_q);
    if(state->workers[w] != NULL)
    {
        state->num_workers += 1;
        ret = 1;
    }

    enif_mutex_unlock(state->lock);

    return ret;
}

int
state_rem_worker(state_ptr state)
{
    worker_ptr worker;
    int w, found;

    // Lock for the whole transaction so we can only remove one
    // worker at a time.

    enif_mutex_lock(state->lock);

    // Check if we have a worker to remove. We keep at least one worker.

    assert(state->num_workers > 0 && "Invalid state: No workers.");
    if(state->num_workers < 2)
    {
        enif_mutex_unlock(state->lock);
        return 0;
    }

    queue_sneak(state->req_q, NULL);

    worker = (worker_ptr) queue_pop(state->dth_q);
    
    // We don't know which worker will die, so we have to be able to
    // remove any worker from our list. This just iterates over and
    // drops the worker. Any workers to the right are shifted one
    // left so there are no holes in the list.

    for(w = 0, found = 0; w < state->num_workers; w++)
    {
        if(state->workers[w] == worker)
        {
            found = 1;
            state->workers[w] = NULL;
        }
        else if(found)
        {
            assert(state->workers[w-1] == NULL && "Invalid state: Worker exists.");
            state->workers[w-1] = state->workers[w];
            state->workers[w] = NULL;
        }
    }

    assert(found && "Invalid state: Failed to find dead worker.");
    worker_destroy(worker);
    state->num_workers -= 1;

    enif_mutex_unlock(state->lock);

    return 1;
}

