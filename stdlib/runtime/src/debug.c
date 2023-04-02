#include <stdio.h>

#include "som_value.h"

void print_value(Value v) {
	#define PRINT_PRIM(fmt, ext) \
		printf("%" fmt #ext "\n", v.as.prim_##ext); \
		break;

	switch(v.type) {
		VAL_PRIM_IU1:
			PRINT_PRIM("%u", iu1);
			break;
		VAL_PRIM_IS1:
			PRINT_PRIM("%d", is1);
			break;
			
		VAL_PRIM_IU8:
			PRINT_PRIM("%u", iu8);
			break;
		VAL_PRIM_IS8:
			PRINT_PRIM("%d", is8);
			break;

		VAL_PRIM_IU16:
			PRINT_PRIM("%u", iu16);
			break;
		VAL_PRIM_IS16:
			PRINT_PRIM("%d", is16);
			break;

		VAL_PRIM_IU32:
			PRINT_PRIM("%u", iu32);
			break;
		VAL_PRIM_IS32:
			PRINT_PRIM("%d", is32);
			break;

		VAL_PRIM_IU64:
			PRINT_PRIM("%lu", iu64);
			break;
		VAL_PRIM_IS64:
			PRINT_PRIM("%ld", is64);
			break;

		VAL_PRIM_IU128:
			PRINT_PRIM("%llu", iu128);
			break;
		VAL_PRIM_IS128:
			PRINT_PRIM("%lld", is128);
			break;

		VAL_PRIM_F16:
			PRINT_PRIM("%f", f16);
			break;
		VAL_PRIM_F32:
			PRINT_PRIM("%f", f32);
			break;
		VAL_PRIM_F64:
			PRINT_PRIM("%lf", f64);
			break;

		VAL_PRIM_V:
			printf("<void>\n");
			break;

		VAL_THUNK:
			printf("<thunk>\n");
			break;

		VAL_TUPLE:
			printf("<tuple>\n");
			break;
	}

	#undef PRINT_PRIM
}