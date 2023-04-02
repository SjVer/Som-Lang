#include "som_types.h"

#pragma once

#define packed __attribute__ ((packed))

typedef struct Value Value;

typedef struct {
	value_ptr function;
	Value* arguments;
} packed Thunk;

typedef struct {
	int size;
	Value* values;
} packed Tuple;

typedef enum {
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

typedef struct Value {
	ValueType type;
	union {
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

#define VAL(vtype, vas, val) (Value){.type = vtype, .as.vas = (vas)(val)}
#define VOID_VAL VAL(VAL_PRIM_V, prim_v, 0)
#define IS32_VAL(v) VAL(VAL_PRIM_IS32, prim_is32, v)
#define IUS_VAL(v) VAL(VAL_PRIM_IUS, prim_ius, v)
#define TUPLE_VAL(count, els) \
	(Value){.type = VAL_TUPLE, .as.tuple = {.size = count, .values = (Value*)(els)}}
#define TAG_TUPLE_VAL(tag) TUPLE_VAL(1, (Value[]){IUS_VAL(tag)})

#define TUPLE_GET(tup, index, vas) tup.values[index].as.vas
#define TUPLE_TAG(tup) TUPLE_GET(tup, 0, prim_ius)
