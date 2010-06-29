#include <stdio.h>
#include "queue.h"

queue_t*
queue_create(void (*dtor) (void*))
{
    queue_t* ret;
    
    if(dtor == NULL) return NULL;

    ret = (queue_t*) enif_alloc(sizeof(queue_t));
    if(ret == NULL) return NULL;
    ret->dtor = dtor;    

    ret->lock = enif_mutex_create("queue_lock");
    if(ret->lock == NULL)
    {
        enif_free(ret);
        return NULL;
    }

    ret->cond = enif_cond_create("queue_cond");
    if(ret->cond == NULL)
    {
        enif_mutex_destroy(ret->lock);
        enif_free(ret);
        return NULL;
    }

    ret->head = NULL;
    ret->tail = NULL;

    return ret;
}

void
queue_destroy(queue_t* queue)
{
    qitem_t* entry;
    while(queue->head != NULL)
    {
        entry = queue->head;
        queue->head = entry->next;
        queue->dtor(entry->data);
        enif_free(entry);
    }

    enif_cond_destroy(queue->cond);
    enif_mutex_destroy(queue->lock);

    enif_free(queue);
}

int
queue_push(queue_t* queue, void* item)
{
    qitem_t* entry = (qitem_t*) enif_alloc(sizeof(qitem_t));
    if(entry == NULL) return 0;

    entry->data = item;
    entry->prev = NULL;
    entry->next = NULL;

    enif_mutex_lock(queue->lock);
    
    if(queue->tail != NULL)
    {
        entry->prev = queue->tail;
        queue->tail->next = entry;
    }
    queue->tail = entry;

    if(queue->head == NULL)
    {
        queue->head = queue->tail;
    }

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return 1;
}

int
queue_sneak(queue_t* queue, void* item)
{
    qitem_t* entry = (qitem_t*) enif_alloc(sizeof(qitem_t));
    if(entry == NULL) return 0;

    entry->data = item;
    entry->prev = NULL;
    entry->next = NULL;

    enif_mutex_lock(queue->lock);

    if(queue->head != NULL)
    {
        entry->next = queue->head;
        queue->head->prev = entry;
    }
    queue->head = entry;

    if(queue->tail == NULL)
    {
        queue->tail = queue->head;
    }

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return 1;
}

void*
queue_pop(queue_t* queue)
{
    qitem_t* entry;
    void* item;

    enif_mutex_lock(queue->lock);
    
    while(queue->head == NULL)
    {
        enif_cond_wait(queue->cond, queue->lock);
    }

    entry = queue->head;
    if(entry->next != NULL)
    {
        queue->head = entry->next;
        queue->head->prev = NULL;
        entry->next = NULL;
    }
    else
    {
        queue->head = NULL;
        queue->tail = NULL;
    }

    enif_mutex_unlock(queue->lock);

    item = entry->data;
    enif_free(entry);

    return item;
}

void*
queue_pop_nowait(queue_t* queue)
{
    qitem_t* entry;
    void* item;

    enif_mutex_lock(queue->lock);

    entry = queue->head;
    if(entry == NULL)
    {
        enif_mutex_unlock(queue->lock);
        return NULL;
    }
    else if(entry->next != NULL)
    {
        queue->head = entry->next;
        queue->head->prev = NULL;
        entry->next = NULL;
    }
    else
    {
        queue->head = NULL;
        queue->tail = NULL;
    }

    enif_mutex_unlock(queue->lock);
    item = entry->data;
    enif_free(entry);

    return item;
}

void
queue_done(queue_t* queue, void* item)
{
    queue->dtor(item);
}
