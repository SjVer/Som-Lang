#include "som_std.h"

#pragma once

#ifndef STDIN_PATH
#   define STDIN_PATH "<stdin>"
#endif
#ifndef STDOUT_PATH
#   define STDOUT_PATH "<stdout>"
#endif
#ifndef STDERR_PATH
#   define STDERR_PATH "<stderr>"
#endif

#define VAL_FILE VAL_TUPLE

#define FILE_MEM_PATH 0
#define FILE_MEM_DESCR 1
#define FILE_MEM_MODE 2

#define FILE_PATH(tup) TUPLE_GET(tup, FILE_MEM_PATH, prim_ius)
#define FILE_DESCR(tup) TUPLE_GET(tup, FILE_MEM_DESCR, prim_is32)
#define FILE_MODE(tup) TUPLE_GET(tup, FILE_MEM_MODE, tuple)

typedef enum {
    IOMODE_READ,
    IOMODE_WRITE,
    IOMODE_APPEND,
    IOMODE_READ_WRITE,
} IOMode;

Value som_stdin();
Value som_stdout();
Value som_stderr();
Value som_openf(Value path, Value mode);
Value som_closef(Value file);
Value som_readf(Value file);
Value som_putsf(Value file, Value str);
