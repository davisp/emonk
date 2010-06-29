#ifndef EMONK_QUEUE_H
#define EMONK_QUEUE_H

#include "erl_nif.h"

typedef struct _qitem_t
{
    struct _qitem_t*    prev;
    struct _qitem_t*    next;
    void*               data;
} qitem_t;


typedef struct
{
    ErlNifMutex*        lock;
    ErlNifCond*         cond;

    qitem_t*            head;
    qitem_t*            tail;
    
    void                (*dtor) (void*);
} queue_t;


queue_t* queue_create(void (*dtor) (void*));
void queue_destroy(queue_t* queue);

int queue_push(queue_t* queue, void* item);
int queue_sneak(queue_t* queue, void* item);
void* queue_pop(queue_t* queue);
void* queue_pop_nowait(queue_t* queue);
void queue_done(queue_t* queue, void* item);

#endif // Included queue.h
