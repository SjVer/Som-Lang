#include "heap.h"
#include "value.h"

value create_float(f64 f) {
    value v = som_malloc_object(8, TAG_FLOAT);
    Val_float(v) = f;
    return v;
}

value som_prim_mul_float(value a, value b) {
    return create_float(Val_float(a) * Val_float(b));
}
