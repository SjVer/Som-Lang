#include "som_types.h"

#pragma once

typedef __uint64_t value;

/*
	Structure:
	00-07: value type (1 byte)
	08-15: gc/value info (1 byte)
	16-30: padding (2 bytes)
	31-63: unboxed value or boxed value size (4 bytes)
	64-..: boxed value
*/

// struct {
// 	value* function;
// 	value* arguments;
// } Thunk;

// struct {
// 	int size;
// 	value* values;
// } Tuple;

enum ValueType {
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
};

#define VAL_TYPE(v) ((ValueType)(v & 0xff))
#define VAL_INFO(v) ((__uint8_t)((v << 8) & 0xff))
#define VAL_VALUE(v) ((v << 24) & 0xffffffff)
#define VAL_SIZE(v) ((__uint8_t)VAL_VALUE(v))

// #define VAL(vtype, vas, val) (Value){.type = vtype, .as.vas = (vas)(val)}
// #define VOID_VAL VAL(VAL_PRIM_V, prim_v, 0)
// #define IS32_VAL(v) VAL(VAL_PRIM_IS32, prim_is32, v)
// #define IUS_VAL(v) VAL(VAL_PRIM_IUS, prim_ius, v)
// #define TUPLE_VAL(count, els) \
// 	(Value){.type = VAL_TUPLE, .as.tuple = {.size = count, .values = (Value*)(els)}}
// #define TAG_TUPLE_VAL(tag) TUPLE_VAL(1, (Value[]){IUS_VAL(tag)})

// #define TUPLE_GET(tup, index, vas) tup.values[index].as.vas
// #define TUPLE_TAG(tup) TUPLE_GET(tup, 0, prim_ius)
