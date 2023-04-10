#include <stdlib.h>

#include "som_std.h"

value som_exit(value status) {
    exit(Val_value(status));
}
