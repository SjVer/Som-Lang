#include <malloc.h>

#include "std.h"

value som_deref(value ptr) {
    return **(value**)Val_data_ptr(ptr);
}

value som_set(value ptr, value val) {
    **(value**)Val_data_ptr(ptr) = val;
    return Null_val;
}

value som_malloc(value size) {
    void* ptr = malloc(Val_value(size));
    value val = malloc(HEADER_SIZE + sizeof(void*));

    *(value*)Val_data_ptr(val) = ptr;
    return val;
}

value som_free(value ptr) {
    free((void*)*Val_data_ptr(ptr));
    return Null_val;
}
