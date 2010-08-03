#ifndef EMONK_WORKER_H
#define EMONK_WORKER_H

#include "erl_nif.h"

#include "queue.h"

typedef enum
{
    worker_init,
    worker_running,
    worker_terminated
} worker_status_e;

typedef struct worker_t* worker_ptr;

worker_ptr worker_create(ErlNifThreadOpts* topts, queue_ptr req_q,
                            queue_ptr dth_q);
void worker_destroy(void* obj);

#endif // Included worker.h
