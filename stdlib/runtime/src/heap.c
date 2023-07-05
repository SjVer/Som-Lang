#include <stdlib.h>
#include <memory.h>

#include "heap.h"

byte som_minor_heap[MINOR_HEAP_SIZE];
byte* som_minor_ptr;

bool som_is_minor(void* ptr) {
	return (byte*)ptr >= som_minor_heap
		&& (byte*)ptr < som_minor_heap + MINOR_HEAP_SIZE;
}

void* som_minor_malloc(size size) {
	if (som_minor_ptr - som_minor_heap < size) {
		Fail("minor heap out of memory");
		// return NULL;
	}
	som_minor_ptr -= size;
	// memset(som_minor_ptr, 0, size);
	return som_minor_ptr;
}

void* som_loose_malloc(size size) {
	void* ptr = malloc(size);
	if (!ptr) Fail_errno();
	return ptr;
}

void* som_malloc(size size) {
    if (som_minor_ptr - som_minor_heap < size)
        return som_minor_malloc(size);
    else
        return som_loose_malloc(size);
}

void som_loose_free(void* ptr) {
	free(ptr);
}

value som_malloc_object(size extra_size, byte tag, ui32 payload) {
	size size = sizeof(header) + extra_size;

	// TODO: check if it fits properly in the minor heap
	object* object = som_minor_malloc(size);

	*object = Hd_with_tag(*object, tag);
	*object = Hd_with_status(*object, STATUS_ALIVE);
    *object = Hd_with_payload(*object, payload);

	return object;
}

CTOR() {
	memset(som_minor_heap, 0, MINOR_HEAP_SIZE);
	som_minor_ptr = som_minor_heap + MINOR_HEAP_SIZE;
}
