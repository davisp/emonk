#ifndef EMONK_STATE_H
#define EMONK_STATE_H

#include <js/jsapi.h>
#include "erl_nif.h"

#include "alias.h"
#include "queue.h"

typedef struct state_t* state_ptr;

state_ptr state_create(ErlNifEnv* env);
void state_destroy(state_ptr state);

queue_ptr state_req_queue(state_ptr state);
ErlNifResourceType* state_res_type(state_ptr state);
JSRuntime* state_js_runtime(state_ptr state);

ENTERM state_ok(state_ptr state);
ENTERM state_error(state_ptr state);

unsigned int state_num_workers(state_ptr state);
int state_add_worker(state_ptr state);
int state_rem_worker(state_ptr state);

#endif // Included state.h
