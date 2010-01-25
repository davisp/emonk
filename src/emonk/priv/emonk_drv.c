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
    ErlDrvTermData undefined;
    ErlDrvTermData bad_cmd;
    emonk_vm_t* vm;
} emonk_drv_t;

int send_undefined(emonk_drv_t* drv, emonk_req_t* req);
int send_response(emonk_drv_t* drv, emonk_req_t* req, void* data, int length);

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
    emonk_settings_t settings;
    
    if(parse_settings(cmd, &settings) < 0)
    {
        ret = ERL_DRV_ERROR_BADARG;
        goto error;
    }
    
    drv = (emonk_drv_t*) driver_alloc(sizeof(emonk_drv_t));
    if(drv == NULL) goto error;

    drv->port = port;
    drv->ok = driver_mk_atom("ok");
    drv->error = driver_mk_atom("error");
    drv->undefined = driver_mk_atom("undefined");
    drv->bad_cmd = driver_mk_atom("bad_command");
    
    drv->vm = init_vm(&settings);
    if(drv->vm == NULL) goto error;
    
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
emonk_control(ErlDrvData handle, uint cmd, char* b, int l, char **rb, int rl)
{
    emonk_drv_t* drv = (emonk_drv_t*) handle;
    emonk_req_t* req = read_req_info(cmd, (unsigned char*) b, l);
    int length;
    int resp;
    
    if(req == NULL)
    {
        *rb[0] = 0;
        return 1;
    }

    void* data = vm_eval(drv->vm, req, &length);
    
    if(data == NULL)
    {
        resp = send_undefined(drv, req);
    }
    else
    {
        resp = send_response(drv, req, data, length);
    }

    if(data != NULL) driver_free(data);
    
    if(resp < 0)
    {
        *rb[0] = 0;
        return 1;
    }
    
    return 0;
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

int
send_undefined(emonk_drv_t* drv, emonk_req_t* req)
{
    ErlDrvTermData terms[] = {
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) req->call_id, req->cid_len,
		ERL_DRV_ATOM, req->ok ? drv->ok : drv->error,
		ERL_DRV_ATOM, drv->undefined,
		ERL_DRV_TUPLE, 3
	};
    return driver_output_term(drv->port, terms, 9);    
}

int
send_response(emonk_drv_t* drv, emonk_req_t* req, void* data, int length)
{
    ErlDrvTermData terms[] = {
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) req->call_id, req->cid_len,
		ERL_DRV_ATOM, req->ok ? drv->ok : drv->error,
		ERL_DRV_EXT2TERM, (ErlDrvTermData) data, length,
		ERL_DRV_TUPLE, 3
	};
    return driver_output_term(drv->port, terms, 10);
}
