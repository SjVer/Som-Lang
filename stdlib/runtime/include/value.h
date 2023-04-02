#include "types.h"
#pragma once

#define packed __attribute__ ((packed))

typedef struct _Thunk {
	value_ptr function;
	struct _Value** arguments;
} packed Thunk;

typedef struct _Tuple {
	int size;
	struct _Value** values;
} packed Tuple;

typedef enum _ValueType {
	VAL_PRIM_IU1,
	VAL_PRIM_IS1,

	VAL_PRIM_IU8,
	VAL_PRIM_IS8,
	
	VAL_PRIM_IU16,
	VAL_PRIM_IS16,
	
	VAL_PRIM_IU32,
	VAL_PRIM_IS32,
	
	VAL_PRIM_IU64,
	VAL_PRIM_IS64,
	
	VAL_PRIM_IU128,
	VAL_PRIM_IS128,
	
	VAL_PRIM_IUS,
	VAL_PRIM_ISS,
	
	VAL_PRIM_F16,
	VAL_PRIM_F32,
	VAL_PRIM_F64,
	
	VAL_PRIM_V,

	VAL_THUNK,
	VAL_TUPLE,
} packed ValueType;

typedef struct _Value {
	ValueType type;
	union {
		prim_v		void_;

		prim_iu1	prim_iu1;
		prim_is1	prim_is1;

		prim_iu8	prim_iu8;
		prim_is8	prim_is8;

		prim_iu16	prim_iu16;
		prim_is16	prim_is16;

		prim_iu32	prim_iu32;
		prim_is32	prim_is32;

		prim_iu64	prim_iu64;
		prim_is64	prim_is64;

		prim_iu128	prim_iu128;
		prim_is128	prim_is128;

		prim_ius	prim_ius;
		prim_iss	prim_iss;

		prim_f16	prim_f16;
		prim_f32	prim_f32;
		prim_f64	prim_f64;

		prim_v		prim_v;

		Thunk		thunk;
		Tuple		tuple;
	} packed as;
} Value;

#define VAL(vtype, vas, val) (Value){.type = vtype, .as.vas = val}
#define VAL_VOID VAL(VAL_PRIM_V, prim_v, 0)