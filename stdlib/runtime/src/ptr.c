#include "std.h"
#include "memory.h"

value som_ptr_deref(value ptr) {
    return **(value**)Val_data_ptr(ptr);
}

value som_ptr_set(value ptr, value val) {
    **(value**)Val_data_ptr(ptr) = val;
    return Null_val;
}

value som_ptr_malloc(value size) {
    void* ptr = som_heap_malloc(Val_value(size));
    value val = som_heap_malloc_object(sizeof(void*), TAG_RAW_DATA);
    
    *(void**)Val_data_ptr(val) = ptr;
    *val = Hd_with_tag(*val, TAG_RAW_DATA);
    return val;
}

value som_ptr_free(value ptr) {
    som_heap_free(*(void**)Val_data_ptr(ptr));
    return Null_val;
}
