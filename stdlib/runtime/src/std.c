#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "std.h"

void _som_fail(const char* message) {
	fprintf(stderr, "Fatal error: %s\n", message);
	fflush(stderr);
	_Exit(errno != 0 ? errno : 1);
}

void _som_fail_errno(int errnum) {
	_som_fail(strerror(errnum));
}