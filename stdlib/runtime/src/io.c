#include <malloc.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "heap.h"
#include "str.h"
#include "io.h"
#include "value.h"

object stdin_obj;
object stdout_obj;
object stderr_obj;

value print_float(value f) {
    printf("%f (status = 0x%02x)\n", Val_float(f), Hd_status(*f));
    return Null_val;
}

value som_io_stdin() {
	return Boxed_value(stdin_obj);
}

value som_io_stdout() {
	return Boxed_value(stdout_obj);
}

value som_io_stderr() {
	return Boxed_value(stderr_obj);
}

value som_io_openf(value path, value mode) {
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

	value file = som_malloc_object(3 * VALUE_SIZE, TAG_RECORD, 3);
	Val_field(file, FILE_PATH) = som_str_copy(path);
	Val_field(file, FILE_DESCR) = Unboxed_val(fd);
	Val_field(file, FILE_MODE) = mode;

    return file;
}

value som_io_closef(value file) {
	close(Val_value(File_descr(file)));
	return Null_val;
}

value som_io_readf(value file) {
	int fd = Val_value(File_descr(file));
	size_t size = lseek(fd, 0, SEEK_END);
	lseek(fd, 0, SEEK_SET);

	char* buf = malloc(size + 1);
	read(fd, buf, size);

	value val = som_str_make(buf);
	free(buf);

	return val;
}

value som_io_putsf(value file, value str) {
	int fd = Val_value(File_descr(file));

	// TODO: check mode and errors
	write(fd, Val_string(str), Hd_payload(*str));
	fsync(fd);

	return Null_val;
}

CTOR() {
	Val_field(som_io_stdin(), FILE_PATH) = som_str_make(STDIN_PATH);
	Val_field(som_io_stdin(), FILE_DESCR) = Unboxed_val(STDIN_FILENO);
	Val_field(som_io_stdin(), FILE_MODE) = malloc(HEADER_SIZE);
	*Val_field(som_io_stdin(), FILE_MODE) = Hd_with_tag(0, IOMODE_READ);

	Val_field(som_io_stdout(), FILE_PATH) = som_str_make(STDOUT_PATH);
	Val_field(som_io_stdout(), FILE_DESCR) = Unboxed_val(STDOUT_FILENO);
	Val_field(som_io_stdout(), FILE_MODE) = malloc(HEADER_SIZE);
	*Val_field(som_io_stdout(), FILE_MODE) = Hd_with_tag(0, IOMODE_APPEND);

	Val_field(som_io_stderr(), FILE_PATH) = som_str_make(STDERR_PATH);
	Val_field(som_io_stderr(), FILE_DESCR) = Unboxed_val(STDERR_FILENO);
	Val_field(som_io_stderr(), FILE_MODE) = malloc(HEADER_SIZE);
	*Val_field(som_io_stderr(), FILE_MODE) = Hd_with_tag(0, IOMODE_APPEND);
}
