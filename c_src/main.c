#include <string.h>

#include "erl_nif.h"

#include "alias.h"
#include "state.h"
#include "vm.h"
#include "worker.h"

static int
load(ErlNifEnv* env, void** priv, ENTERM load_info)
{
    state_ptr state;
    
    JS_SetCStringsAreUTF8();

    state = state_create(env);
    if(state == NULL) return -1;

    *priv = (void*) state;

    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    state_destroy((state_ptr) priv);
}

static ENTERM
num_workers(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    return enif_make_uint(env, state_num_workers(state));
}

static ENTERM
add_worker(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);

    if(!state_add_worker(state))
    {
        return state_error(state);
    }

    return state_ok(state);
}

static ENTERM
rem_worker(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    
    if(!state_rem_worker(state))
    {
        return state_error(state);
    }

    return state_ok(state);
}

static ENTERM
create_ctx(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    unsigned int stack_size;
    vm_ptr vm;
    ENTERM ret;

    if(argc != 1 || !enif_get_uint(env, argv[0], &stack_size))
    {
        return enif_make_badarg(env);
    }

    vm = vm_init(state, (size_t) stack_size);
    if(vm == NULL) return state_error(state);
    
    ret = enif_make_resource(env, vm);
    enif_release_resource(vm);
    
    return enif_make_tuple2(env, state_ok(state), ret);
}

static ErlNifFunc nif_funcs[] = {
    {"add_worker", 0, add_worker},
    {"rem_worker", 0, rem_worker},
    {"num_workers", 0, num_workers},
    {"create_ctx", 1, create_ctx},
    {"eval", 4, vm_add_eval},
    {"call", 5, vm_add_call}
};

ERL_NIF_INIT(emonk, nif_funcs, &load, NULL, NULL, unload);

