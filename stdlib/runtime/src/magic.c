#include "value.h"

value som_magic_add(value a, value b) {
    int ia = Val_value(a);
    int ib = Val_value(b);
    return Unboxed_val(ia + ib);
}

value som_magic_eq(value a, value b) {
    // TODO: this is shit, obviously
    long is_eq = a == b;
    return (value)is_eq;
}
