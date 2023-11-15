#include <stdarg.h>
#include <stdlib.h>
#include <math.h>

#include "heap.h"
#include "value.h"

value som_make_float(f64 f) {
    value v = som_malloc_object(8, TAG_FLOAT, 0);
    Val_float(v) = f;
    return v;
}

value som_rem_float(value lhs, value rhs) {
    return som_make_float(fmod(Val_float(lhs), Val_float(rhs)));
}

value som_abs_int(value val) {
    return Unboxed_val(abs(Val_value(val)));
}

value som_abs_float(value val) {
    return som_make_float(fabs(Val_float(val)));
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

value som_eval_closure(value val) {
    object closure = Val_object(val);
    ui32 argc = Hd_payload(closure);
    void* fptr = Val_field(val, 0);

    switch (argc) {
        case 1: 
            return ((value (*)(value))fptr)(
                Val_field(val, 1)
            );
        case 2: 
            return ((value (*)(value, value))fptr)(
                Val_field(val, 1),
                Val_field(val, 2)
            );
        case 3: 
            return ((value (*)(value, value, value))fptr)(
                Val_field(val, 1),
                Val_field(val, 2),
                Val_field(val, 3)
            );
        default: 
            Fail("INVALID COLSURE ARGC");
    }
}