#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "std.h"

void som_fail(const char* message) {
	fprintf(stderr, "Fatal error: %s\n", message);
	fflush(stderr);
	_Exit(errno != 0 ? errno : 1);
}

void som_fail_match() {
	som_fail("unmatched value");
}

void som_fail_errno(int errnum) {
	som_fail(strerror(errnum));
}

// entrypoint

extern value (*som_entrypoint)(value);

int main(int argc, const char** argv) {
	// TODO: construct args list
	value args = Unboxed_val(argc);
	
	value res = (*som_entrypoint)(args);
	printf("exit code: %ld\n", Val_value(res));

	return 0;
}