#define JEMALLOC_NO_RENAME
#include <jemalloc/jemalloc.h>

#include "memory.h"

void* som_heap_malloc(size size) {
	return je_malloc(size);
}

void som_heap_free(void* ptr) {
	je_free(ptr);
}

value som_heap_malloc_object(size extra_size, byte tag) {
	size size = sizeof(header) + extra_size;

	// TODO: check if it fits properly in the minor heap
	object* object = som_heap_malloc(size);
	*object = Hd_with_tag(*object, tag);
	*object = Hd_with_status(*object, STATUS_ALIVE);

	return object;
}