#include <malloc.h>
#include "som_std.h"

Value som_deref(Value ptr) {
    ASSERT_TYPE(ptr, VAL_PRIM_IUS);
    return *((Value*)ptr.as.prim_ius);
}

Value som_set(Value ptr, Value val) {
    ASSERT_TYPE(ptr, VAL_PRIM_IUS);
    *((Value*)ptr.as.prim_ius) = val;
    return VAL_VOID;
}

Value som_malloc(Value size) {
    ASSERT_TYPE(size, VAL_PRIM_IUS);
    // TODO: multiply size by sizeof(Value)?
    void* ptr = malloc(size.as.prim_ius);
    return VAL(VAL_PRIM_IUS, prim_ius, (prim_ius)ptr);
}

Value som_free(Value ptr) {
    ASSERT_TYPE(ptr, VAL_PRIM_IUS);
    free((void*)ptr.as.prim_ius);
    return VAL_VOID;
}