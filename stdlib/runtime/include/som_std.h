#include "som_value.h"

#pragma once

#define CTOR void __attribute__((constructor)) som_CTOR
#define DTOR void __attribute__((destructor)) som_DTOR

// #ifdef TAG_ASSERTIONS
// #include <assert.h>
// #define Assert_tag(value, tag) \
// 	assert(!Is_unboxed(value) && Hd_tag(*value) == tag)
// #else
// #define Assert_tag(value, tag)
// #endif