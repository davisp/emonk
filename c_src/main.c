#include <string.h>

#include "erl_nif.h"

#include "alias.h"
#include "state.h"
#include "worker.h"

static int
load(ErlNifEnv* env, void** priv, ENTERM load_info)
{
    state_t* state = state_create(env);
    if(state == NULL)
    {
        return -1;
    }

    *priv = (void*) state;
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    state_t* state = (state_t*) priv;
    state_destroy(state);
}

static ENTERM
add_worker(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);

    if(!state_add_worker(state))
    {
        return state->atom_error;
    }

    return state->atom_ok;
}

static ENTERM
rem_worker(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    if(!state_rem_worker(state))
    {
        return state->atom_error;
    }

    return state->atom_ok;
}

static ENTERM
num_workers(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    return enif_make_int(env, state_num_workers(state));
}

static ENTERM
create_ctx(ErlNifEnv* env, int argc, CENTERM argv[0])
{
    state_t* state = (state_t*) enif_priv_data(env);
    size_t stack_size;
    vm_t* vm;
    ENTERM ret;

    if(argc != 1 || !enif_get_uint(env, argv[0], &stack_size))
    {
        return enif_make_badarg(env);
    }

    enif_mutex_lock(state->lock);
    vm = enif_alloc_resource(state->ctx_res, sizeof(vm_t));
    if(vm == NULL) return enif_make_badarg(env);
    
    ret = enif_make_resource(env, vm);
    enif_release_resource(vm);

    if(!vm_init(vm, state->runtime, stack_size))
    {
        return enif_make_badarg(env);
    }

    enif_mutex_unlock(state->lock);

    return enif_make_tuple2(env, state->atom_ok, ret);
}

static ENTERM
eval(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    vm_t* vm;
    job_t* job;
    req_t* req;
    ErlNifBinary script;

    if(argc != 4) return enif_make_badarg(env);
    
    if(!enif_get_resource(env, argv[0], state->ctx_res, (void**) &vm))
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

    enif_mutex_lock(vm->lock);

    if(!queue_push(vm->jobs, job))
    {
        enif_mutex_unlock(vm->lock);
        goto error;
    }
    job = NULL;

    if(vm->status == vm_idle)
    {
        req = req_create(req_exec, vm);
        if(!queue_push(state->requests, req))
        {
            enif_mutex_unlock(vm->lock);
            goto error;
        }
        req = NULL;
    }

    enif_mutex_unlock(vm->lock);

    return state->atom_ok;

error:
    if(job != NULL) job_destroy(job);
    if(req != NULL) req_destroy(req);
    return enif_make_badarg(env);
}

static ENTERM
call(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    vm_t* vm;
    job_t* job;
    req_t* req;

    if(argc != 5) return enif_make_badarg(env);
    
    if(!enif_get_resource(env, argv[0], state->ctx_res, (void**) &vm))
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

    enif_mutex_lock(vm->lock);

    if(!queue_push(vm->jobs, job))
    {
        enif_mutex_unlock(vm->lock);
        goto error;
    }
    job = NULL;

    if(vm->status == vm_idle)
    {
        req = req_create(req_exec, vm);
        if(!queue_push(state->requests, req))
        {
            enif_mutex_unlock(vm->lock);
            goto error;
        }
        req = NULL;
    }

    enif_mutex_unlock(vm->lock);

    return state->atom_ok;

error:
    if(job != NULL) job_destroy(job);
    if(req != NULL) req_destroy(req);
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = {
    {"add_worker", 0, add_worker},
    {"rem_worker", 0, rem_worker},
    {"num_workers", 0, num_workers},
    {"create_ctx", 1, create_ctx},
    {"eval", 4, eval},
    {"call", 5, call}
};

ERL_NIF_INIT(emonk, nif_funcs, &load, NULL, NULL, unload);

