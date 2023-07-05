#include "heap.h"
#include "value.h"

value som_prim_mul_float(value a, value b) {
    return som_make_float(Val_float(a) * Val_float(b));
}
