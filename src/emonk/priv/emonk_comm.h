#ifndef EMONK_COMM_H
#define EMONK_COMM_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <jsapi.h>
#include <erl_driver.h>

#define SMALL_INTEGER 'a'
#define INTEGER       'b'
#define FLOAT         'c'
#define ATOM          'd'
#define NEW_FLOAT     'F'
#define SMALL_TUPLE   'h'
#define LARGE_TUPLE   'i'
#define NIL           'j'
#define STRING        'k'
#define LIST          'l'
#define BINARY        'm'
#define SMALL_BIG     'n'
#define LARGE_BIG     'o'

#define VERSION_MAGIC 131

typedef struct _emonk_req_t
{
    char ok;

    char* call_id;
    int cid_len;

    char* script;
    int scr_len;
    
    char* function;    
    int argc;
    jsval* argv;
} emonk_req_t;

emonk_req_t* read_req_info(JSContext* cx, uint cmd, unsigned char* buf, int len);
void* free_req_info(emonk_req_t* req);

void* to_erl(JSContext* cx, jsval val, int* length);
jsval to_js(JSContext* cx, char* data, int* remaining);

void mk_error(JSContext* cx, const char* mesg, JSErrorReport* report);


#endif // Include emonk_comm.h