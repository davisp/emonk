#include <assert.h>
#include <stdio.h>

#include "queue.h"

struct qitem_t
{
    struct qitem_t*     next;
    void*               data;
};

typedef struct qitem_t* qitem_ptr;

struct queue_t
{
    ErlNifMutex*        lock;
    ErlNifCond*         cond;
    qitem_ptr           head;
    qitem_ptr           tail;
    int                 length;
};

queue_ptr
queue_create(const char* name)
{
    queue_ptr ret;

    ret = (queue_ptr) enif_alloc(sizeof(struct queue_t));
    if(ret == NULL) goto error;

    ret->lock = NULL;
    ret->cond = NULL;
    ret->head = NULL;
    ret->tail = NULL;
    ret->length = 0;

    ret->lock = enif_mutex_create("queue_lock");
    if(ret->lock == NULL) goto error;
    
    ret->cond = enif_cond_create("queue_cond");
    if(ret->cond == NULL) goto error;

    return ret;

error:
    if(ret->lock != NULL) enif_mutex_destroy(ret->lock);
    if(ret->cond != NULL) enif_cond_destroy(ret->cond);
    if(ret != NULL) enif_free(ret);
    return NULL;
}

void
queue_destroy(queue_ptr queue)
{
    ErlNifMutex* lock;
    ErlNifCond* cond;
    int length;

    enif_mutex_lock(queue->lock);
    lock = queue->lock;
    cond = queue->cond;
    length = queue->length;

    queue->lock = NULL;
    queue->cond = NULL;
    queue->head = NULL;
    queue->tail = NULL;
    queue->length = -1;
    enif_mutex_unlock(lock);

    assert(length == 0 && "Attempting to destroy a non-empty queue.");
    enif_cond_destroy(cond);
    enif_mutex_destroy(lock);
    enif_free(queue);
}

int
queue_has_item(queue_ptr queue)
{
    int ret;

    enif_mutex_lock(queue->lock);
    ret = (queue->head != NULL);
    enif_mutex_unlock(queue->lock);
    
    return ret;
}

int
queue_push(queue_ptr queue, void* item)
{
    qitem_ptr entry = (qitem_ptr) enif_alloc(sizeof(struct qitem_t));
    if(entry == NULL) return 0;

    entry->data = item;
    entry->next = NULL;

    enif_mutex_lock(queue->lock);

    assert(queue->length >= 0 && "Invalid queue size at push");
    
    if(queue->tail != NULL)
    {
        queue->tail->next = entry;
    }

    queue->tail = entry;

    if(queue->head == NULL)
    {
        queue->head = queue->tail;
    }

    queue->length += 1;

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return 1;
}

int
queue_sneak(queue_ptr queue, void* item)
{
    qitem_ptr entry = (qitem_ptr) enif_alloc(sizeof(struct qitem_t));
    if(entry == NULL) return 0;

    entry->data = item;
    entry->next = NULL;

    enif_mutex_lock(queue->lock);

    assert(queue->length >= 0 && "Invalid queue size at sneak");

    if(queue->head != NULL)
    {
        entry->next = queue->head;
    }

    queue->head = entry;

    if(queue->tail == NULL)
    {
        queue->tail = queue->head;
    }

    queue->length += 1;

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return 1;
}

void*
queue_pop(queue_ptr queue)
{
    qitem_ptr entry;
    void* item;

    enif_mutex_lock(queue->lock);
    
    // Wait for an item to become available.
    while(queue->head == NULL)
    {
        enif_cond_wait(queue->cond, queue->lock);
    }
    
    assert(queue->length >= 0 && "Invalid queue size at pop.");

    // Woke up because queue->head != NULL
    // Remove the entry and return the payload.

    entry = queue->head;
    queue->head = entry->next;
    entry->next = NULL;

    if(queue->head == NULL)
    {
        assert(queue->tail == entry && "Invalid queue state: Bad tail.");
        queue->tail = NULL;
    }

    queue->length -= 1;

    enif_mutex_unlock(queue->lock);

    item = entry->data;
    enif_free(entry);

    return item;
}

void*
queue_pop_nowait(queue_ptr queue)
{
    qitem_ptr entry;
    void* item;

    enif_mutex_lock(queue->lock);

    assert(queue->length >= 0 && "Invalid queue size at pop_nowait.");

    // Nothing available. Bail out.
    if(queue->head == NULL)
    {
        enif_mutex_unlock(queue->lock);
        return NULL;
    }

    entry = queue->head;
    queue->head = entry->next;
    entry->next = NULL;
    
    if(queue->head == NULL)
    {
        assert(queue->tail == entry && "Invalid queue state: Bad tail.");
        queue->tail = NULL;
    }

    queue->length -= 1;

    enif_mutex_unlock(queue->lock);

    item = entry->data;
    enif_free(entry);

    return item;
}

