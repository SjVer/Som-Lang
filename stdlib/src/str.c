#include <stdlib.h>
#include <string.h>

#include "som_stdlib.h"

som_Int som_length(som_Str str) {
    return (som_Int)strlen(str);
}

som_Str som_concat(som_Str str1, som_Str str2) {
   // TODO: error checking
   char* result = malloc(strlen(str1) + strlen(str2) + 1);

   strcpy(result, str1);
   strcat(result, str2);
   return (som_Str)result;
}