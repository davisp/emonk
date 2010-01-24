#ifndef EMONK_COMM_H
#define EMONK_COMM_H

#include <jsapi.h>

void* to_erl(JSContext* cx, jsval val, int* length);
jsval to_js(JSContext* cx, char* data, int* remaining);

#endif // Include emonk_comm.h