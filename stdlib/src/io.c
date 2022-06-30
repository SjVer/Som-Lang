#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "somtypes.h"

void print(som_str format, ...) {
	va_list args;
    va_start(args, format);
    printf(format, args);
    va_end(args);
}

void println(som_str format, ...) {
	va_list args;
    va_start(args, format);
    printf(format, args);
    va_end(args);
	putchar('\n');
}

som_lng open(som_str path, som_str mode) {
	// put it in file array or smth instead?
	return (som_int)fopen(path, mode);
}

som_nll close(som_lng file) {
	fclose((FILE*)file);
}

som_str read(som_lng file) {
	char* buffer = 0;
	long length;
	FILE* f = (FILE*)file;

	if(f) {
		fseek(f, 0, SEEK_END);
		length = ftell(f);
		fseek(f, 0, SEEK_SET);
		
		buffer = malloc(length);
		if(buffer) fread(buffer, 1, length, f);
		fseek(f, 0, SEEK_SET);
	}

	return buffer;
}

som_nll write(som_lng file, som_str text) {
	fprintf((void*)file, "%s", text);
}