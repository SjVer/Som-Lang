#include "som_std.h"

#pragma once

#define Val_string(v) Obj_data_ptr(char*, Val_object(v))

value copy_str(value str);

value som_length(value str);
value som_concat(value str1, value str2);
