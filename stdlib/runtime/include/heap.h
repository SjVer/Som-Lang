#pragma once

#include "std.h"

#define MINOR_HEAP_SIZE 1028

typedef long size;

bool som_is_minor(void* ptr);
bool som_is_major(void* ptr);
bool som_is_loose(void* ptr);

void* som_minor_malloc(size size);
void* som_major_malloc(size size);
void* som_loose_malloc(size size);
void* som_malloc(size size);

void som_loose_free(void* ptr);

value som_malloc_object(size extra_size, byte tag, ui32 payload);
