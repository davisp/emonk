
#include "job.h"

job_t*
job_create()
{
    job_t* ret = (job_t*) enif_alloc(sizeof(job_t));
    if(ret == NULL) return NULL;

    ret->type = job_unknown;
    ret->env = enif_alloc_env();
    if(ret->env == NULL) goto error;

    ret->ref = 0;
    ret->script.data = NULL;
    ret->script.size = 0;
    ret->error = 0;

    return ret;

error:
    if(ret->env != NULL) enif_free_env(ret->env);
    enif_free(ret);
    return NULL;
}

void
job_destroy(void* obj)
{
    job_t* job = (job_t*) obj;
    if(job->script.data != NULL) enif_release_binary(&job->script);
    if(job->env != NULL) enif_free_env(job->env);
    enif_free(job);
}

