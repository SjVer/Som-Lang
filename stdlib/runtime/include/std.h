#pragma once

#include <errno.h>

#include "value.h"

#define CTOR void __attribute__((constructor)) som_CTOR
#define DTOR void __attribute__((destructor)) som_DTOR

// #ifdef TAG_ASSERTIONS
// #include <assert.h>
// #define Assert_tag(value, tag) \
// 	assert(!Is_unboxed(value) && Hd_tag(*value) == tag)
// #else
// #define Assert_tag(value, tag)
// #endif

void _som_fail(const char* message);
void _som_fail_errno(int errnum);

#define DEFAULT_FAIL_MSG "fatal error"

#define Fail(message) (_som_fail(message))
#define Just_fail() (_som_fail(DEFAULT_FAIL_MSG))
#define Fail_errno() (_som_fail_errno(errno))
