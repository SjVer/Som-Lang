#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "std.h"

void som_fail(const char* message) {
	fprintf(stderr, "Fatal error: %s\n", message);
	fflush(stderr);
	_Exit(errno != 0 ? errno : 1);
}

void som_fail_errno(int errnum) {
	som_fail(strerror(errnum));
}