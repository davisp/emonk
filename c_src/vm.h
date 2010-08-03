#ifndef EMONK_VM_H
#define EMONK_VM_H

#include <js/jsapi.h>
#include "erl_nif.h"

#include "alias.h"
#include "state.h"
#include "vm.h"

typedef enum
{
    vm_idle,
    vm_resume,
    vm_running
} vm_status_e;

typedef struct vm_t* vm_ptr;

vm_ptr vm_init(state_ptr state, size_t stack_size);
void vm_destroy(ErlNifEnv* env, void* obj);
vm_status_e vm_set_status(vm_ptr vm, vm_status_e status);

ENTERM vm_add_eval(ErlNifEnv* env, int argc, CENTERM argv[]);
ENTERM vm_add_call(ErlNifEnv* env, int argc, CENTERM argv[]);
void vm_run_job(vm_ptr vm, queue_ptr req_q);

void vm_set_error(void* obj, ENBINARY mesg, ENBINARY src, unsigned int line);

#endif // Included vm.h
