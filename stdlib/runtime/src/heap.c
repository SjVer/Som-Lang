#include <stdlib.h>
#include <memory.h>
#include <stdio.h>

#include "std.h"
#include "heap.h"

#define DEBUG_HEAP_CHECKS

#ifdef DEBUG_HEAP
#	define Debug(...) printf("=== HEAP === " __VA_ARGS__)
#	define Check_heap() s_check_heap()
#else
#	define Debug(...)
#	define Check_heap()
#endif

struct heap s_heap;

// alias for readability
typedef chunk_hd chunk;

chunk* s_get_wilderness() {
	chunk_ft* footer = (chunk_ft*)(s_heap.end - sizeof(chunk_ft));
	return footer->header;
}

chunk_ft* s_get_footer(chunk_hd* header) {
	return (chunk_ft*)((byte*)header + sizeof(chunk_hd) + header->size);
}

void s_create_footer(chunk_hd* header) {
	chunk_ft* footer = s_get_footer(header);
	footer->header = header;
}

heap_bin* s_find_bin(size size) {
	long og_size = size;
	// must at least be big enough for the chunk
	size = size < OVERHEAD ? OVERHEAD : size;

	int index = 0;
	while (size >>= 1) index++;
	index -= 2;

	if (index > HEAP_BIN_COUNT - 1) index = HEAP_BIN_COUNT - 1;
	return s_heap.bins + index;
}

void s_add_to_bin(heap_bin* bin, chunk_hd* header) {
	header->next = NULL;
	header->prev = NULL;

	if (bin->head == NULL) {
		// bin was empty, this header is the first
		bin->head = header;
		return;
	}

	chunk_hd* curr = bin->head;
	chunk_hd* prev = NULL;

	// find shit
	while (curr != NULL && curr->size <= header->size) {
		prev = curr;
		curr = curr->next;
	}

	if (curr != NULL && prev != NULL) {
		// in the middle
		header->next = curr;
		curr->prev = header;
		header->prev = prev;
		prev->next = header;
	}
	else if (curr != NULL && prev == NULL) {
		// as first or second element
		if (bin->head->size > header->size)
			bin->head->next = header;
		else {
			header->next = bin->head;
			bin->head->prev = header;
			bin->head = header;
		}
	}
	else if (curr == NULL && prev != NULL) {
		// at end of the list
		prev->next = header;
		header->prev = prev;
	}
}

void s_remove_from_bin(heap_bin* bin, chunk_hd* header) {
	if (bin->head == NULL) return;
	if (bin->head == header) {
		bin->head = bin->head->next;
		return;
	}

	chunk_hd* temp = bin->head->next;
	while (temp != NULL) {
		if (temp == header) {
			// found the node
			if (temp->next == NULL) {
				// last item
				temp->prev->next = NULL;
			}
			else {
				// middle item
				temp->prev->next = temp->next;
				temp->next->prev = temp->prev;
			}
			return;
		}
		temp = temp->next;
	}
}

chunk_hd* s_get_best_fit(heap_bin* bin, size size) {
    if (bin->head == NULL) return NULL;

    chunk_hd* temp = bin->head;

    while (temp != NULL) {
        if (temp->size >= size) {
			// found a fit!
            return temp;
        }
        temp = temp->next;
    }
    return NULL;
}

// =========================================

void s_check_heap() {
#ifdef DEBUG_HEAP_CHECKS
	Debug("checking heap\n");
	Debug("  chunks: %ld (including wilderness)\n", s_heap.chunk_count);
	Debug("  used: %ld/%ld bytes (%ld free)\n",
		s_heap.used, s_heap.end - s_heap.start, s_heap.size - s_heap.used);
#endif
#define Check(cond, what) if (! (cond)) Fail(HEAP_CORR_MSG " (invalid " what ")");

	Check(s_heap.end - s_heap.start == s_heap.size, "bounds");
	Check(s_heap.size >= HEAP_MIN_SIZE && s_heap.size <= HEAP_MAX_SIZE, "size");
	Debug("  expecting wilderness footer at: %p\n", s_heap.end - sizeof(chunk_ft));
	chunk* w = s_get_wilderness();
	Debug("  wilderness at: %p\n", w);
	Check(w && s_get_footer(w)->header == w, "wilderness");

#undef Check
#ifdef DEBUG_HEAP_CHECKS
	Debug("  heap OK\n");
#endif
}

void s_grow_heap() {
	Debug("growing heap\n");

	// keep track of the wilderness bc its footer
	// will no longer be at the end after realloc
	chunk* wilderness = s_get_wilderness();

	// check new size
	size new_size = s_heap.size + HEAP_GROW_SIZE;
	if (new_size > HEAP_MAX_SIZE) Fail(OUT_OF_MEM_MSG);

	// reallocate heap
	s_heap.start = (long)realloc((void*)s_heap.start, new_size);
	if (s_heap.start == 0) Fail_errno();
	s_heap.end = s_heap.start + new_size;
	s_heap.size = new_size;

	/*
		FIXME:

			right now the new wilderness footer just isn't in
			the correct spot (the wilderness header should not
			have moved, so we can assume it's in its correct
			place).

			the deviation seems to have something to do with
			the allocated size of an allocation that triggers
			a heap growth, but i'm not sure how or why that'd be.

			maybe consult the rubber duck?
	*/
	
	// resize wilderness
	printf("%p: %ld -> ", wilderness, wilderness->size);
	wilderness->size += HEAP_GROW_SIZE;
	s_create_footer(wilderness);
	printf("%p: %ld\n", s_get_wilderness(), wilderness->size);
	Debug("  new wilderness footer: %p\n", s_get_footer(wilderness));
	Debug("  new wilderness: %p\n", s_get_footer(wilderness)->header);

	Debug("  heap grown %d bytes\n", HEAP_GROW_SIZE);
	Check_heap();
}

void s_shrink_heap() {
	// TODO
}

// =========================================

void _som_init_heap() {
	// just to be sure
	memset(s_heap.bins, 0, sizeof(heap_bin) * HEAP_BIN_COUNT);

	// initially, the heap is one big wilderness
	void* region = malloc(HEAP_INIT_SIZE);
	if (region == NULL) Fail_errno();
	memset(region, 0, HEAP_INIT_SIZE);

	chunk* wilderness = (chunk_hd*)region;
	wilderness->free = true;
	wilderness->size = HEAP_INIT_SIZE - OVERHEAD;
	s_create_footer(wilderness);

	s_add_to_bin(s_find_bin(wilderness->size), wilderness);

	s_heap.start = (long)region;
	s_heap.end = (long)region + HEAP_INIT_SIZE;
	s_heap.size = HEAP_INIT_SIZE;
	s_heap.used = 0;
	s_heap.chunk_count = 1;

	Debug("initialized heap (%d bytes)\n", HEAP_INIT_SIZE);
	Debug("  overhead: %ld bytes\n", OVERHEAD);
	Check_heap();
}

void _som_free_heap() {
	Check_heap();
	free((void*)s_heap.start);
}

void* _som_heap_alloc(size size) {
	Check_heap();
	Debug("allocating %ld bytes\n", size);
	if (size > HEAP_MAX_ALLOC) Fail("invalid allocation size");

	// find a probably correct bin
	heap_bin* bin = s_find_bin(size);
	chunk* chunk = s_get_best_fit(bin, size);

	// find a chunk large enough
	while (chunk == NULL) {
		if (bin - s_heap.bins >= HEAP_BIN_COUNT - 1) {
			// found no fitting chunk
			Debug("  no free chunk found!\n");
			Fail(OUT_OF_MEM_MSG);
		}
		
		bin += 1;
		chunk = s_get_best_fit(bin, size);
	}

	Debug("  found chunk in bin %ld: %p (%ld bytes)\n", 
		bin - s_heap.bins, chunk, chunk->size);

	// split the chunk if it's way to big
	if (chunk->size - size > OVERHEAD + HEAP_MIN_ALLOC) {
		// create a new chunk just after the current one
		chunk_hd* split = (void*)chunk + OVERHEAD + size;
		split->size = chunk->size - size - OVERHEAD;
		split->free = true;
		s_create_footer(split);

		Debug("  split chunk %p (%ld bytes)\n", split, split->size);

		// add it to its bin
		s_add_to_bin(s_find_bin(split->size), split);

		s_heap.chunk_count++;
		
		chunk->size = size;
		s_create_footer(chunk);
	}

	chunk->free = false;
	s_remove_from_bin(bin, chunk);

	// determine if the heap should grow or shrink
	chunk_hd* wilderness = s_get_wilderness();
	if (wilderness->size < HEAP_MIN_WILDERNESS) {
		Debug("  wilderness too small (%ld/%d bytes)\n",
			wilderness->size, HEAP_MIN_WILDERNESS);
		s_grow_heap();
	}
	else if (wilderness->size > HEAP_MAX_WILDERNESS) {
		Debug("  wilderness too large (%ld/%d bytes)\n",
			wilderness->size, HEAP_MAX_WILDERNESS);
		s_shrink_heap();
	}

	if (chunk == wilderness) Fail(HEAP_CORR_MSG);
	else s_heap.used += size + OVERHEAD;

	chunk->next = NULL;
	chunk->prev = NULL;

	void* ptr = (void*)chunk + sizeof(chunk_hd);
	Debug("  returned address: %p (%ld bytes)\n", ptr, chunk->size);
	Debug("  done\n");
	return ptr;
}

void _som_heap_free(void* ptr) {
	chunk_hd* header = ptr - sizeof(chunk_hd);

	if (header == s_get_wilderness()) Fail(HEAP_CORR_MSG);
	else s_heap.used -= header->size + OVERHEAD;

	Debug("freeing chunk %p (%ld bytes)\n", header, header->size);

	// if this is the first chunk there's
	// no need to coalesce it
	if ((long)header == s_heap.start) {
		s_add_to_bin(s_find_bin(header->size), header);
		Check_heap();
		return;
	}

	chunk_hd* next = (chunk_hd*)((void*)s_get_footer(header) + sizeof(chunk_ft));
	chunk_hd* prev = ((chunk_ft*)((void*)header - sizeof(chunk_ft)))->header;

	// try to coalesce
	if (prev->free) {
		heap_bin* bin = s_find_bin(prev->size);
		s_remove_from_bin(bin, prev);
		s_heap.chunk_count--;

		prev->size += OVERHEAD + header->size;
		s_create_footer(prev);

		// we're now working with what was the prev. chunk
		header = prev;
	}
	if (next->free) {
		heap_bin* bin = s_find_bin(prev->size);
		s_remove_from_bin(bin, next);
		s_heap.chunk_count--;

		header->size += OVERHEAD + next->size;

		// clear out next chunk
		chunk_ft* old_footer = s_get_footer(next);
		old_footer->header = 0;
		next->size = 0;
		next->free = 0;

		s_create_footer(header);
	}

	// mark the chunk as free
	header->free = true;
	s_add_to_bin(s_find_bin(header->size), header);

	Check_heap();
}

// =========================================

value _som_heap_alloc_object(size extra_size, byte tag) {
	size size = sizeof(header) + extra_size;

	// TODO: check if it fits properly in the minor heap
	object* object = _som_heap_alloc(size);
	*object = Hd_with_tag(*object, tag);
	*object = Hd_with_status(*object, STATUS_ALIVE);

	return object;
}

CTOR() { _som_init_heap(); }
DTOR() { _som_free_heap(); }
