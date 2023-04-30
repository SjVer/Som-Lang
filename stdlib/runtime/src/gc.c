#include <malloc.h>

#include "std.h"
#include "gc.h"

ObjectHeap minor_heap;

void init_heap(ObjectHeap* heap, size size_in_bytes, float grow_factor) {
	heap->capacity = size_in_bytes / HEADER_SIZE;
	heap->slots = malloc(size_in_bytes);
	heap->count = 0;

	for (int i = 0; i < heap->capacity; i++) {
		heap->slots[i] = Hd_with_info(0, STATUS_DEAD);
	}
}

object* find_free_slot(ObjectHeap* heap) {
	int slot = 0;
	for (;slot < heap->capacity; slot++)
		if (Hd_info(heap->slots[slot]) == STATUS_DEAD)
			return heap->slots + slot;

	// no free space, grow heap
	size new_capacity = heap->capacity * heap->grow_factor;
	realloc(heap->slots, new_capacity);

	// return the first new slot
	return heap->slots + slot + 1;
}

value alloc_object(unsigned field_count) {
	object* object = find_free_slot(&minor_heap);
	minor_heap-> BUSY HERE

	for (int i = 0; i < field_count; i++)
		
}

CTOR() {
	init_heap(&minor_heap, 100000, 1.5);
}