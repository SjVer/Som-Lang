#include <stdlib.h>

#include "std.h"

value som_exit(value status) {
    exit(Val_value(status));
}
