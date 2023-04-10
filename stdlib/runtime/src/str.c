#include <malloc.h>
#include <string.h>

#include <stdio.h>

#include "str.h"

value make_str(const char* str) {
	ui32 size = strlen(str);
	value val = (value)malloc(HEADER_SIZE + size);

	*val = Hd_with_payload(Hd_with_tag(0, TAG_STRING), size);
	strcpy(Val_string(val), str);
	return val;
}

value copy_str(value str) {
	ui32 size = Hd_payload(*str);
	value new_str = (value)malloc(HEADER_SIZE + size);

	*str = Hd_with_payload(Hd_with_tag(0, TAG_STRING), size);
	strcpy(Val_string(new_str), Val_string(str));
	return new_str;
}

value som_length(value str) {
	ui32 size = Hd_payload(*str);
	return Unboxed_val(size);
}

value som_concat(value str1, value str2) {
	ui32 size = Hd_payload(*str1) + Hd_payload(*str2);
	value new_str = (value)malloc(HEADER_SIZE + size);

	Hd_with_payload(Hd_with_tag(0, TAG_STRING), size);
	strcpy(Val_string(new_str), Val_string(str1));
	strcat(Val_string(new_str), Val_string(str2));
	return new_str;
}

