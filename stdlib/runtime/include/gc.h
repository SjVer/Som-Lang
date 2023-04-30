#include "value.h"

#define STATUS_DEAD 0x00
#define STATUS_DYING 0x01
#define STATUS_ALIVE 0x02

typedef __ssize_t size;

typedef struct {
	size capacity;
	float grow_factor;
	size count;
	object* slots;
} ObjectHeap;

void init_heap(ObjectHeap* heap, size size_in_bytes, float grow_factor);

value alloc_object(unsigned field_count);