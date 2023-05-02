#pragma once

#include <errno.h>

#include "value.h"

#define __PASTE(a, b, c) a ## b ## c
#define _PASTE(a, b, c) __PASTE(a, b, c)
#define _CTOR(c, l) void __attribute__((constructor)) _PASTE(som_CTOR_, c, l)
#define _DTOR(c, l) void __attribute__((destructor)) _PASTE(som_CTOR_, c, l)

#define CTOR _CTOR(__COUNTER__, __LINE__)
#define DTOR _DTOR(__COUNTER__, __LINE__)

void som_fail(const char* message);
void som_fail_errno(int errnum);

#define DEFAULT_FAIL_MSG "fatal error"

#define Fail(message) (som_fail(message))
#define Just_fail() (som_fail(DEFAULT_FAIL_MSG))
#define Fail_errno() (som_fail_errno(errno))
