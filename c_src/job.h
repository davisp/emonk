#ifndef EMONK_JOB_H
#define EMONK_JOB_H

#include "erl_nif.h"

#include "alias.h"
#include "vm.h"

typedef enum
{
    job_unknown,
    job_eval,
    job_call
} job_type_e;

typedef struct _job_t
{
    job_type_e      type;

    ErlNifEnv*      env;
    ENTERM          ref;
    ErlNifPid       pid;
    
    ErlNifBinary    script;
    ENTERM          name;
    ENTERM          args;

    ENTERM          error;
} job_t;


job_t* job_create();
void job_destroy(void* obj);

#endif // Included job.h
