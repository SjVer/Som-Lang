#include <stdarg.h>
#include "heap.h"
#include "value.h"

value som_make_float(f64 f) {
    value v = som_malloc_object(8, TAG_FLOAT, 0);
    Val_float(v) = f;
    return v;
}

value som_make_closure(void* func, i32 arity, i32 argc, ...) {
    size size = (2 + argc) * VALUE_SIZE; 
    value v = som_malloc_object(size, TAG_CLOSURE, argc);

    Val_field(v, 0) = func;
    Val_field(v, 1) = Unboxed_val(arity);

    va_list args;
    va_start(args, argc);
    for (int i = 0; i < argc; i++)
        Val_field(v, i + 2) = va_arg(args, value);
    va_end(args);

    return v;
}
