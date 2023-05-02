#include <stdio.h>

#define JEMALLOC_NO_RENAME
#include "../jemalloc/include/jemalloc/jemalloc-som.h"

#include "memory.h"

void* som_heap_malloc(size size) {
	return som_je_malloc(size);
}

void som_heap_free(void* ptr) {
	som_je_free(ptr);
}

value som_heap_malloc_object(size extra_size, byte tag) {
	size size = sizeof(header) + extra_size;

	// TODO: check if it fits properly in the minor heap
	object* object = som_heap_malloc(size);
	*object = Hd_with_tag(*object, tag);
	*object = Hd_with_status(*object, STATUS_ALIVE);

	return object;
}

void som_print_je_stats(void* cbopaque, const char* s) {
	char* hint = (char*)cbopaque;
	printf("%s: %s\n", hint, s);
}

CTOR() {
	som_je_malloc_stats_print(som_print_je_stats, "HEAP", "Jg");
}