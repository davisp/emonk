#ifndef EMONK_STATE_H
#define EMONK_STATE_H

#include <js/jsapi.h>
#include "erl_nif.h"

#include "queue.h"
#include "worker.h"

typedef struct
{
    ErlNifMutex*            lock;
    
    ErlNifResourceType*     ctx_res;
    
    queue_t*                requests;
    queue_t*                deaths;

    worker_t*               workers;
    ErlNifThreadOpts*       thread_opts;
    
    JSRuntime*              runtime;

    ENTERM                  atom_ok;
    ENTERM                  atom_error;
} state_t;


state_t* state_create(ErlNifEnv* env);
void state_destroy(state_t* state);

int state_add_worker(state_t* state);
int state_rem_worker(state_t* state);
int state_num_workers(state_t* state);

#endif // Included state.h
