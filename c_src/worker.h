#ifndef EMONK_WORKER_H
#define EMONK_WORKER_H

#include "erl_nif.h"

#include "queue.h"
#include "req.h"

typedef enum
{
    worker_init,
    worker_running,
    worker_terminated
} worker_status_e;

typedef struct _worker_t
{
    ErlNifTid           tid;
    ErlNifMutex*        lock;

    worker_status_e     status;
    queue_t*            requests;
    queue_t*            deaths;

    struct _worker_t*   prev;
    struct _worker_t*   next;
} worker_t;

worker_t* worker_create(ErlNifThreadOpts* opts, queue_t* reqs, queue_t* deaths);
void worker_destroy(void* obj);

#endif // Included worker.h
