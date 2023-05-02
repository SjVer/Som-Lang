#include "std.h"

#pragma once

#define TAG_STRING TAG_RAW_DATA
#define Val_string(v) Obj_data_ptr(char*, Val_object(v))

value som_str_make(const char* str);
value som_str_copy(value str);

value som_str_length(value str);
value som_str_concat(value str1, value str2);
