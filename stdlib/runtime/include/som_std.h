#include "som_value.h"

#pragma once

#define CTOR void __attribute__((constructor)) som_CTOR
#define DTOR void __attribute__((destructor)) som_DTOR

#ifdef TYPE_ASSERTIONS
#include <assert.h>
#define ASSERT_TYPE(value, type) assert(value.type == type)
#else
#define ASSERT_TYPE(value, type)
#endif