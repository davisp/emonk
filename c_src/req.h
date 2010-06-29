#ifndef EMONK_REQ_H
#define EMONK_REQ_H

#include "erl_nif.h"

#include "vm.h"

typedef enum
{
    req_exec,
    req_close
} req_type_e;

typedef struct _req_t
{
    req_type_e      type;
    vm_t*           vm;

    struct _req_t*  prev;
    struct _req_t*  next;
} req_t;

req_t* req_create(req_type_e type, vm_t* vm);
void req_destroy(void* obj);

#endif // Included req.h
