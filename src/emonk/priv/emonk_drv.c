/* author Kevin Smith <ksmith@basho.com>
   copyright 2009-2010 Basho Technologies

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

#include <string.h>

#include "emonk.h"
#include "emonk_util.h"

typedef struct _emonk_drv_t
{
  ErlDrvPort port;
  ErlDrvTermData ok;
  ErlDrvTermData error;
  ErlDrvTermData unknown_cmd;
  emonk_vm_t* vm;
  int shutdown;
} emonk_drv_t;

static int
emonk_init()
{
    if(!JS_CStringsAreUTF8()) JS_SetCStringsAreUTF8();
    return 0;
}

static ErlDrvData
emonk_start(ErlDrvPort port, char *cmd)
{
    uint rt_max, gc_max, gc_last, ctx;
    emonk_drv_t* drv = NULL;
    ErlDrvData ret = ERL_DRV_ERROR_GENERAL;
    
    if(parse_settings(cmd, &rt_max, &gc_max, &gc_last, &ctx) < 0)
    {
        ret = ERL_DRV_ERROR_BADARG;
        goto error;
    }
    
    drv = (emonk_drv_t*) driver_alloc(sizeof(emonk_drv_t));
    if(drv == NULL) goto error;

    drv->port = port;
    drv->ok = driver_mk_atom("ok");
    drv->error = driver_mk_atom("error");
    drv->unknown_cmd = driver_mk_atom("unknown_command");
    
    drv->vm = init_vm(rt_max, gc_max, gc_last, ctx);
    if(drv->vm == NULL) goto error;
    
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    
    return (ErlDrvData) drv;

error:
    if(drv != NULL) driver_free(drv);
    return ret;
}

static void
emonk_stop(ErlDrvData handle)
{
    emonk_drv_t* drv = (emonk_drv_t*) handle;
    stop_vm(drv->vm);
    driver_free(drv);
}

static int
emonk_control(ErlDrvData handle, uint cmd,
                char* buf, int len, char **rbuf, int rlen)
{
    emonk_drv_t* drv = (emonk_drv_t*) handle;
    ErlDrvBinary* ret = NULL;

    void* data = vm_eval(drv->vm, buf, len, &rlen);
    if(data == NULL) return 0;

    for(len = 0; len < rlen; len++)
    {
        fprintf(stderr, "%c\n", ((char*) data)[len]);
    }

    ret = driver_alloc_binary(rlen);
    if(ret == NULL) return -1;
    memcpy(ret->orig_bytes, data, rlen);
    driver_free(data);

    return rlen;
}

static ErlDrvEntry
emonk_drv_entry = {
    emonk_init,                         /* init */
    emonk_start,                        /* startup */
    emonk_stop,                         /* shutdown */
    NULL,                               /* output */
    NULL,                               /* ready_input */
    NULL,                               /* ready_output */
    (char *) "emonk_drv",               /* the name of the driver */
    NULL,                               /* finish */
    NULL,                               /* handle */
    emonk_control,                      /* control */
    NULL,                               /* timeout */
    NULL,                               /* process */
    NULL,                               /* ready_async */
    NULL,                               /* flush */
    NULL,                               /* call */
    NULL,                               /* event */
    ERL_DRV_EXTENDED_MARKER,            /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,     /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,     /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING       /* ERL_DRV_FLAGs */
};

DRIVER_INIT(emonk_drv) {
  return &emonk_drv_entry;
}

