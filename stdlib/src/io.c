#include <stdio.h>
#include "types.h"

#ifndef STDIN_PATH
#   define STDIN_PATH "stdin"
#endif
#ifndef STDOUT_PATH
#   define STDOUT_PATH "stdout"
#endif
#ifndef STDERR_PATH
#   define STDERR_PATH "stderr"
#endif

// types

typedef enum {
    IOMode_read,
    IOMode_write,
    IOMode_append,
    IOMode_read_write,
} som_IOMode;

typedef struct {
    som_Str path;
    som_IOMode _mode;
    FILE* _file;
} som_File;

// files

som_File som_stdin;
som_File som_stdout;
som_File som_stderr;

// functions

som_File som_openf(som_Str path, som_IOMode mode) {
    char* modes;
    switch (mode) {
        case IOMode_read:
            modes = "r";
            break;
        case IOMode_write:
            modes = "w";
            break;
        case IOMode_append:
            modes = "a";
            break;
        case IOMode_read_write:
            modes = "w+";
            break;
    };

    FILE* file_ptr = fopen(path, modes);

    if (file_ptr == NULL) {
        // TODO: error handling
    }

    som_File file;
    strcpy(path, file.path);
    file._file = file_ptr;
    file._mode = mode;

    return file;
}

void som_closef(som_File file) {
    // TODO: error handling
    fclose(file._file);
}

void som_putsf(som_File file, som_Str str) {
    // TODO: error handling
    fputs(str, file._file);
}

// ctors

void __attribute__((constructor)) _Som_std_io__ctor() {
    som_stdin = (som_File) {
        .path = STDIN_PATH,
        ._mode = IOMode_append,
        ._file = stdin
    };
    som_stdout = (som_File) {
        .path = STDOUT_PATH,
        ._mode = IOMode_read,
        ._file = stdout
    };
    som_stderr = (som_File) {
        .path = STDERR_PATH,
        ._mode = IOMode_read,
        ._file = stderr
    };
}

void __attribute__((destructor)) _Som_std_io__dtor() {
    som_closef(som_stdin);
    som_closef(som_stdout);
    som_closef(som_stderr);
}
