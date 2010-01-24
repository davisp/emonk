
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "emonk_util.h"

#define MIN(a, b) ((a) < (b) ? (a) : (b))

int
parse_settings(char* cmd, uint* rt_max, uint* gc_max, uint* gc_last, uint* ctx)
{
    size_t idx = 0;
    size_t len = strlen(cmd);

    *rt_max = RT_MAX_BYTES;  // 1MiB
    *gc_max = GC_MAX_BYTES;  // 8MiB
    *gc_last = GC_MAX_MALLOC; // 8MiB
    *ctx = CONTEXT_STACK;        // 8KiB

    // Seek past driver name
    for(idx; idx < len && cmd[idx] != ' '; idx++) {}
    if(idx+1 >= len) return 0;
    idx += 1;

    if(sscanf(cmd + idx, "rt=%u gcmb=%u gcld=%u ctx=%u",
            rt_max, gc_max, gc_last, ctx) != 4)
    {
        return -1;
    }

    if(*rt_max > SETTING_MAX
            || *gc_max > SETTING_MAX
            || *gc_last > SETTING_MAX
            || *ctx > SETTING_MAX)
    {
        return -1;
    }
    
    return 0;
}
