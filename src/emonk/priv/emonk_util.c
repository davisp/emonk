
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "emonk_util.h"

#define MIN(a, b) ((a) < (b) ? (a) : (b))

int
parse_settings(char* cmd, emonk_settings_t* settings)
{
    size_t idx = 0;
    size_t len = strlen(cmd);

    settings->rt_max_bytes = RT_MAX_BYTES;      // 1MiB
    settings->gc_max_bytes = GC_MAX_BYTES;      // 8MiB
    settings->gc_max_malloc = GC_MAX_MALLOC;    // 8MiB
    settings->context_stack = CONTEXT_STACK;    // 8KiB

    // Seek past driver name
    for(idx; idx < len && cmd[idx] != ' '; idx++) {}
    if(++idx >= len) return 0;

    if(sscanf(
        cmd + idx,
        "rt=%u gcmb=%u gcld=%u ctx=%u",
        &settings->rt_max_bytes,
        &settings->gc_max_bytes,
        &settings->gc_max_malloc,
        &settings->context_stack
    ) != 4)
    {
        return -1;
    }

    if(settings->rt_max_bytes > SETTING_MAX
        || settings->gc_max_bytes > SETTING_MAX
        || settings->gc_max_malloc > SETTING_MAX
        || settings->context_stack > SETTING_MAX)
    {
        return -1;
    }
    
    return 0;
}

