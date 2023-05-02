#pragma once

#include <errno.h>

#include "value.h"

#define __PASTE(a, b) a ## b
#define _PASTE(a, b) __PASTE(a, b)
#define _CTOR(c) void __attribute__((constructor)) _PASTE(som_CTOR_, c)
#define _DTOR(c) void __attribute__((destructor)) _PASTE(som_CTOR_, c)

#define CTOR _CTOR(__COUNTER__)
#define DTOR _DTOR(__COUNTER__)

void som_fail(const char* message);
void som_fail_errno(int errnum);

#define DEFAULT_FAIL_MSG "fatal error"

#define Fail(message) (som_fail(message))
#define Just_fail() (som_fail(DEFAULT_FAIL_MSG))
#define Fail_errno() (som_fail_errno(errno))
