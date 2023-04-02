#include <stdlib.h>

#include "som_std.h"

Value som_exit(Value status) {
    ASSERT_TYPE(status, VAL_PRIM_IS32);
    
    exit(status.as.prim_is32);
}
