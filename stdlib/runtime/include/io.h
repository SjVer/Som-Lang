#include "std.h"

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

#define FILE_PATH 0
#define FILE_DESCR 1
#define FILE_MODE 2

#define File_path(v) Val_field(v, FILE_PATH)
#define File_descr(v) Val_field(v, FILE_DESCR)
#define File_mode(v) Val_field(v, FILE_MODE)

enum {
	IOMODE_READ,
	IOMODE_WRITE,
	IOMODE_APPEND,
	IOMODE_READ_WRITE,
};

value som_io_stdin();
value som_io_stdout();
value som_io_stderr();
value som_io_openf(value path, value mode);
value som_io_closef(value file);
value som_io_readf(value file);
value som_io_putsf(value file, value str);
