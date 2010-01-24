
#ifndef EMONK_UTIL_H
#define EMONK_UTIL_H

#define RT_MAX_BYTES 1048576
#define GC_MAX_BYTES 8388608
#define GC_MAX_MALLOC 8388608
#define CONTEXT_STACK 8192
#define SETTING_MAX 1073741824


int
parse_settings(char* cmd, uint* rt_max, uint* gc_max, uint* gc_last, uint* ctx);


#endif // Included util.h