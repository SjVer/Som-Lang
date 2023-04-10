#include <malloc.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "str.h"
#include "io.h"

object stdin_obj;
object stdout_obj;
object stderr_obj;

value som_stdin() {
	return Boxed_value(stdin_obj);
}

value som_stdout() {
	return Boxed_value(stdout_obj);
}

value som_stderr() {
	return Boxed_value(stderr_obj);
}

value som_openf(value path, value mode) {
    int flags;
    switch (Hd_tag(*mode)) {
        case IOMODE_READ:
            flags = O_RDONLY;
            break;
        case IOMODE_WRITE:
			flags = O_WRONLY | O_CREAT | O_TRUNC;
            break;
        case IOMODE_APPEND:
            flags = O_WRONLY | O_CREAT | O_APPEND;
            break;
        case IOMODE_READ_WRITE:
            flags = O_RDWR | O_CREAT | O_TRUNC;
            break;
    };

    // TODO: error handling
	int fd = open(Val_string(path), flags, 438);

	value file = malloc(HEADER_SIZE + VALUE_SIZE * 3);
	Val_field(file, FILE_PATH) = copy_str(path);
	Val_field(file, FILE_DESCR) = Unboxed_val(fd);
	Val_field(file, FILE_MODE) = mode;

    return file;
}

value som_closef(value file) {
	close(Val_value(File_descr(file)));
	return Void_val;
}

value som_readf(value file) {
	int fd = Val_value(File_descr(file));
	size_t size = lseek(fd, 0, SEEK_END);
	lseek(fd, 0, SEEK_SET);

	char* buf = malloc(size + 1);
	read(fd, buf, size);

	value val = make_str(buf);
	free(buf);

	return val;
}

value som_putsf(value file, value str) {
	int fd = Val_value(File_descr(file));

	// TODO: check mode and errors
	write(fd, Val_string(str), Hd_payload(*str));
	fsync(fd);

	return Void_val;
}

CTOR() {
	Val_field(som_stdin(), FILE_PATH) = make_str(STDIN_PATH);
	Val_field(som_stdin(), FILE_DESCR) = Unboxed_val(STDIN_FILENO);
	Val_field(som_stdin(), FILE_MODE) = malloc(HEADER_SIZE);
	*Val_field(som_stdin(), FILE_MODE) = Hd_with_tag(0, IOMODE_READ);

	Val_field(som_stdout(), FILE_PATH) = make_str(STDOUT_PATH);
	Val_field(som_stdout(), FILE_DESCR) = Unboxed_val(STDOUT_FILENO);
	Val_field(som_stdout(), FILE_MODE) = malloc(HEADER_SIZE);
	*Val_field(som_stdout(), FILE_MODE) = Hd_with_tag(0, IOMODE_APPEND);

	Val_field(som_stderr(), FILE_PATH) = make_str(STDERR_PATH);
	Val_field(som_stderr(), FILE_DESCR) = Unboxed_val(STDERR_FILENO);
	Val_field(som_stderr(), FILE_MODE) = malloc(HEADER_SIZE);
	*Val_field(som_stderr(), FILE_MODE) = Hd_with_tag(0, IOMODE_APPEND);
}