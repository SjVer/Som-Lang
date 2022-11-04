#include "types.h"

void exit(int) __attribute__((noreturn));

void som_exit(som_Int status) {
    exit(status);
}