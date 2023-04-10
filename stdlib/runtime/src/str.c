#include <malloc.h>
#include <string.h>

#include <stdio.h>

#include "str.h"

value copy_str(value str) {
	Assert_type(str, TYPE_RAW_DATA);

	ui32 size = Hd_payload(*str);
	value new_str = (value)malloc(size + HEADER_SIZE);

	*str = Hd_with_payload(Hd_with_type(0, TYPE_RAW_DATA), size);
	strcpy(Val_string(new_str), Val_string(str));
	return new_str;
}

value som_length(value str) {
	Assert_type(str, TYPE_RAW_DATA);

	ui32 size = Hd_payload(*str);
	return Unboxed_value(size);
}

value som_concat(value str1, value str2) {
	Assert_type(str1, TYPE_RAW_DATA);
	Assert_type(str2, TYPE_RAW_DATA);

	ui32 size = Hd_payload(*str1) + Hd_payload(*str2);
	value new_str = (value)malloc(size + HEADER_SIZE);

	Hd_with_payload(Hd_with_type(0, TYPE_RAW_DATA), size);
	strcpy(Val_string(new_str), Val_string(str1));
	strcat(Val_string(new_str), Val_string(str2));
	return new_str;
}

