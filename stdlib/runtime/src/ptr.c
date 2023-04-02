#include <malloc.h>

#include "som_std.h"

#define VAL_PTR VAL_PRIM_IUS

Value som_deref(Value ptr) {
    ASSERT_TYPE(ptr, VAL_PTR);

    return *((Value*)ptr.as.prim_ius);
}

Value som_set(Value ptr, Value val) {
    ASSERT_TYPE(ptr, VAL_PTR);

    *((Value*)ptr.as.prim_ius) = val;
    return VOID_VAL;
}

Value som_malloc(Value size) {
    ASSERT_TYPE(size, VAL_PRIM_IUS);

    // TODO: multiply size by sizeof(Value)?
    void* ptr = malloc(size.as.prim_ius);
    return IUS_VAL(ptr);
}

Value som_free(Value ptr) {
    ASSERT_TYPE(ptr, VAL_PTR);

    free((void*)ptr.as.prim_ius);
    return VOID_VAL;
}
