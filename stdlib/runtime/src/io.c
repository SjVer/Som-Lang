#include <malloc.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "str.h"
#include "io.h"

Value stdin_tup[3];
Value stdout_tup[3];
Value stderr_tup[3];

Value som_stdin() {
	return TUPLE_VAL(3, stdin_tup);
}

Value som_stdout() {
	return TUPLE_VAL(3, stdout_tup);
}

Value som_stderr() {
	return TUPLE_VAL(3, stderr_tup);
}

Value som_openf(Value path, Value mode) {
	ASSERT_TYPE(path, VAL_STR);
	ASSERT_TYPE(mode, VAL_TUPLE);

    int flags;
    switch (TUPLE_TAG(mode.as.tuple)) {
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
	int fd = open(STR_GET(path), flags, 438);

	Value* file_tup = malloc(sizeof(Value) * 3);
	file_tup[FILE_MEM_PATH] = copy_str(path);
	file_tup[FILE_MEM_DESCR] = IS32_VAL(fd);
	file_tup[FILE_MEM_MODE] = mode;

    return TUPLE_VAL(3, file_tup);
}

Value som_closef(Value file) {
	ASSERT_TYPE(file, VAL_TUPLE);

	close(FILE_DESCR(file.as.tuple));
	return VOID_VAL;
}

Value som_readf(Value file) {
	ASSERT_TYPE(file, VAL_TUPLE);

	int fd = FILE_DESCR(file.as.tuple);
	size_t size = lseek(fd, 0, SEEK_END);
	lseek(fd, 0, SEEK_SET);

	char* buf = malloc(size + 1);
	read(fd, buf, size);

	return STR_VAL(buf);
}

Value som_putsf(Value file, Value str) {
	ASSERT_TYPE(file, VAL_TUPLE);
	ASSERT_TYPE(str, VAL_STR);

	int fd = FILE_DESCR(file.as.tuple);
	char* c_str = (char*)str.as.prim_ius;
	size_t size = strlen(c_str);

	// TODO: check mode and errors
	write(fd, c_str, size);
	fsync(fd);

	return VOID_VAL;
}

CTOR() {
	stdin_tup[FILE_MEM_PATH] = STR_VAL(STDIN_PATH);
	stdin_tup[FILE_MEM_DESCR] = IS32_VAL(STDIN_FILENO);
	stdin_tup[FILE_MEM_MODE] = TAG_TUPLE_VAL(IOMODE_READ);

	stdout_tup[FILE_MEM_PATH] = STR_VAL(STDOUT_PATH);
	stdout_tup[FILE_MEM_DESCR] = IS32_VAL(STDOUT_FILENO);
	stdout_tup[FILE_MEM_MODE] = TAG_TUPLE_VAL(IOMODE_APPEND);

	stderr_tup[FILE_MEM_PATH] = STR_VAL(STDERR_PATH);
	stderr_tup[FILE_MEM_DESCR] = IS32_VAL(STDERR_FILENO);
	stderr_tup[FILE_MEM_MODE] = TAG_TUPLE_VAL(IOMODE_APPEND);
}