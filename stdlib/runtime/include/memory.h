#pragma once

#include "value.h"

typedef long size;

void* som_heap_malloc(size size);
void som_heap_free(void* ptr);

value som_heap_malloc_object(size extra_size, byte tag);