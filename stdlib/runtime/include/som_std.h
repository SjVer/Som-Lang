#include "som_value.h"

#pragma once

#define CTOR void __attribute__((constructor)) som_CTOR
#define DTOR void __attribute__((destructor)) som_DTOR

#ifdef TYPE_ASSERTIONS
#include <assert.h>
#define Assert_type(value, type) \
	assert(!Is_unboxed(value) && Val_obj_type(value) == type)
#else
#define Assert_type(value, type)
#endif