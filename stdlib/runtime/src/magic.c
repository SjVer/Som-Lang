#include "value.h"

value som_magic_add(value a, value b) {
    int ia = Val_value(a);
    int ib = Val_value(b);
    return Unboxed_val(ia + ib);
}
