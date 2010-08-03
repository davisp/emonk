#include <assert.h>
#include <time.h>

#include "util.h"
#include "vm.h"
#include "worker.h"

struct worker_t
{
    ErlNifTid           tid;
    ErlNifMutex*        lock;

    worker_status_e     status;
    queue_ptr           req_q;
    queue_ptr           dth_q;
};


void* worker_run(void* arg);
void worker_exec(vm_ptr vm);

worker_ptr
worker_create(ErlNifThreadOpts* topts, queue_ptr req_q, queue_ptr dth_q)
{
    worker_ptr worker = (worker_ptr) enif_alloc(sizeof(struct worker_t));
    if(worker == NULL) return NULL;

    worker->lock = enif_mutex_create("worker_lock");
    if(worker->lock == NULL)
    {
        enif_free(worker);
        return NULL;
    }

    worker->status = worker_init;
    worker->req_q = req_q;
    worker->dth_q = dth_q;

    if(enif_thread_create("", &worker->tid, worker_run, worker, topts) != 0)
    {
        enif_mutex_destroy(worker->lock);
        enif_free(worker);
        return NULL;
    }

    return worker;
}

void
worker_destroy(void* obj)
{
    worker_ptr worker = (worker_ptr) obj;
    void* resp;
    
    enif_mutex_lock(worker->lock);

    assert(worker->status == worker_terminated && "Invalid worker: Not dead.");

    enif_mutex_unlock(worker->lock);
    enif_mutex_destroy(worker->lock);

    enif_thread_join(worker->tid, &resp);

    enif_free(worker);
    return;
}

void*
worker_run(void* arg)
{
    worker_ptr self = (worker_ptr) arg;
    queue_ptr req_q = self->req_q;
    queue_ptr dth_q = self->dth_q;
    vm_ptr vm;

    enif_mutex_lock(self->lock);
    self->status = worker_running;
    enif_mutex_unlock(self->lock);

    while(1)
    {
        vm = queue_pop(req_q);

        // Time to close up shop.
        if(vm == NULL)
        {
            enif_mutex_lock(self->lock);
            self->status = worker_terminated;
            enif_mutex_unlock(self->lock);
            
            queue_push(dth_q, self);
            
            return NULL;
        }
        
        vm_run_job(vm, req_q);
    }
}

