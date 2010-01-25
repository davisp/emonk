
#ifndef EMONK_UTIL_H
#define EMONK_UTIL_H

#define RT_MAX_BYTES 1048576
#define GC_MAX_BYTES 8388608
#define GC_MAX_MALLOC 8388608
#define CONTEXT_STACK 8192
#define SETTING_MAX 1073741824

typedef struct _emonk_settings_t
{
    uint rt_max_bytes;
    uint gc_max_bytes;
    uint gc_max_malloc;
    uint context_stack;
} emonk_settings_t;

int parse_settings(char* cmd, emonk_settings_t* settings);

#endif // Included util.h