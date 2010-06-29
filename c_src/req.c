#include "req.h"

req_t*
req_create(req_type_e type, vm_t* vm)
{
    req_t* ret = (req_t*) enif_alloc(sizeof(req_t));
    if(ret == NULL) return ret;

    ret->type = type;
    ret->vm = vm;

    return ret;
}

void
req_destroy(void* obj)
{
    enif_free(obj);
}
