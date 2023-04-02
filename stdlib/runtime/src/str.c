#include <malloc.h>
#include <string.h>

#include "str.h"

Value copy_str(Value str) {
	char* c_str = (char*)str.as.prim_ius;
	size_t size = strlen(c_str) + 1;

	char* dest = malloc(sizeof(char) * size);
	strcpy(dest, c_str);

	return STR_VAL(dest);
}

Value som_length(Value str) {
	ASSERT_TYPE(str, VAL_STR);

    size_t len = strlen((char*)str.as.prim_ius);
	return IUS_VAL(len);
}

Value som_concat(Value str1, Value str2) {
	ASSERT_TYPE(str1, VAL_STR);
	ASSERT_TYPE(str2, VAL_STR);

	char* c_str1 = (char*)str1.as.prim_ius;
	char* c_str2 = (char*)str2.as.prim_ius;

	// TODO: error checking
	char* result = malloc(strlen(c_str1) + strlen(c_str2) + 1);

	strcpy(result, c_str1);
	strcat(result, c_str2);
	return STR_VAL(result);
}
