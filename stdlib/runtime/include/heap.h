#pragma once

#include "value.h"
#include <stdbool.h>

// #define HEAP_INIT_SIZE 100000000
#define HEAP_INIT_SIZE 10000
#define HEAP_MAX_SIZE 500000000
#define HEAP_MIN_SIZE 10000
#define HEAP_GROW_SIZE 50000

#define HEAP_MIN_WILDERNESS 2000
#define HEAP_MAX_WILDERNESS HEAP_MAX_SIZE

#define HEAP_MIN_ALLOC 4
#define HEAP_MAX_ALLOC HEAP_MIN_WILDERNESS

#define HEAP_BIN_COUNT 9

#define HEAP_CORR_MSG "heap corrupted"
#define OUT_OF_MEM_MSG "out of memory"

typedef __ssize_t size;

/*
	Adapted from: https://github.com/CCareaga/heap_allocator

	Heap design:
		Each chunk of memory has a chunk header struct at
		the begining and a footer struct at the end. The
		header holds two pointers used in the doubly-linked
		list ([next] and [prev]). The footer struct simply
		holds a pointer to the header (used while freeing
		adjacent chunks). The chunk at the end of the
		heap is called the "wilderness" chunk. It is the
		largest chunk and its min and max sizes are defined
		in heap.h. contracting and expanding the heap is
		as easy as resizing this wilderness chunk. Free
		chunks of memory are stored in "bins" each bin is
		actually just a doubly-linked lists of nodes with
		similar sizes.

	Note:
		The [size] field of a chunk header contains the
		size of the chunk's contents, so excluding the
		sizes of its header and footer.
*/

typedef struct chunk_hd {
	struct chunk_hd* prev;
	struct chunk_hd* next;
	bool free;
	size size;
} chunk_hd;

typedef struct {
	chunk_hd* header;
} chunk_ft;

typedef struct {
	chunk_hd* head;
} heap_bin;

struct heap {
	long start, end;
	size size, used;
	size chunk_count;
	heap_bin bins[HEAP_BIN_COUNT];
};

#define OVERHEAD (sizeof(chunk_ft) + sizeof(chunk_hd))

void _som_init_heap();
void _som_free_heap();

void* _som_heap_alloc(size size);
void _som_heap_free(void* ptr);

value _som_heap_alloc_object(size extra_size, byte tag);
