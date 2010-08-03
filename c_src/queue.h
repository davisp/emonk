#ifndef EMONK_QUEUE_H
#define EMONK_QUEUE_H

#include "erl_nif.h"

typedef struct queue_t* queue_ptr;

queue_ptr queue_create(const char* name);
void queue_destroy(queue_ptr queue);

int queue_has_item(queue_ptr queue);

int queue_push(queue_ptr queue, void* item);
int queue_sneak(queue_ptr queue, void* item);
void* queue_pop(queue_ptr queue);
void* queue_pop_nowait(queue_ptr queue);

#endif // Included queue.h
