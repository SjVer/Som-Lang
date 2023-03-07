#include "som_stdlib.h"

typedef void* som_Any; // ???

som_Any som_deref(som_Ptr ptr) {
    return (som_Any)(*ptr); // ??????
}

som_Nll som_set(som_Ptr ptr, som_Any val) {
    *ptr = val; // ??????
}

som_Ptr som_malloc(som_Sze size) {
    // TODO: error checking?
    return malloc(size);
}

som_Nll som_free(som_Ptr ptr) {
    // TODO: error checking?
    free(ptr);
}