#include <string.h>
#include <stdio.h>

void* _som_heap_alloc(size_t);
void* _som_heap_free(size_t);

#define heap_alloc _som_heap_alloc
#define heap_free _som_heap_free

void som_grow_heap() {}

int main() {
	// const char* lit = "Hello World!";
	
	// char* str = som_heap_alloc(strlen(lit) + 1);
	// printf("str: %p\n", str);

	// strcpy(str, lit);
	// printf("str value: %s\n", str);

	// void* arr = som_heap_alloc(1024);
	// printf("arr: %p\n", arr);


	for (int i = 0; i < 10; i++) {
		printf("\n");
		heap_alloc(1024);
	}
	
	// som_heap_free(arr);

	return 0;
}