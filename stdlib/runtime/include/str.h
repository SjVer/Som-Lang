#include "som_std.h"

#pragma once

#define VAL_STR VAL_PRIM_IUS
#define STR_VAL(str) VAL(VAL_STR, prim_ius, str)
#define STR_GET(str) (char*)str.as.prim_ius

Value copy_str(Value str);

Value som_length(Value str);
Value som_concat(Value str1, Value str2);
