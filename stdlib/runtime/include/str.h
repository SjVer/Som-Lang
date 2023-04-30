#include "std.h"

#pragma once

#define TAG_STRING TAG_RAW_DATA
#define Val_string(v) Obj_data_ptr(char*, Val_object(v))

value _som_make_str(const char* str);
value _som_copy_str(value str);

value som_length(value str);
value som_concat(value str1, value str2);
