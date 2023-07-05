#include <stdarg.h>
#include "heap.h"
#include "value.h"

value som_make_float(f64 f) {
    value v = som_malloc_object(8, TAG_FLOAT, 0);
    Val_float(v) = f;
    return v;
}

value som_make_closure(void* func, i32 argc, ...) {
    size size = VALUE_SIZE + argc * VALUE_SIZE; 
    value v = som_malloc_object(size, TAG_CLOSURE, argc);

    Val_field(v, 0) = func;

    va_list args;
    va_start(args, argc);
    for (int i = 0; i < argc; i++)
        Val_field(v, i + 1) = va_arg(args, value);

    return v;
}
