#include "som_types.h"

#pragma once

typedef __uint8_t byte;
typedef __uint32_t ui32;
typedef __uint64_t ui64;

/*
	Value type:
		type `value` is either a 8-bit aligned
		pointer to an allocated object or an
		unboxed primitive of less than 64 bits,
		indicated by the 64'th bit (or 32 bits
		in case of a 32-bit architecture)

	Object header structure:
		00-07: value type/tag (1 byte)
		08-15: gc/value info (1 byte)
		16-30: padding (2 bytes)
		31-63: payload (4 bytes)

	Raw data objects:
		payload: data size in bytes
		following data: data as array

	Tuple objects:
		payload: element count
		following data: array of values
	
	Thunk objects:
		payload: argument count
		following data: function pointer and values

	Tagged objects:
		payload: element count
		following data: array of values
*/

// header definitions

typedef ui64 header;

#define VALUE_SIZE __WORDSIZE
#define HEADER_SIZE 8

#define HD_T_MASK ((1ull << 8) - 1ull)
#define HD_I_MASK (((1ull << 8) - 1ull) << 8)
#define HD_P_MASK (((1ull << 32) - 1ull) << 32)

#define Hd_type(h) ((byte)((h) & HD_T_MASK))
#define Hd_with_type(h, t) (((h) & ~HD_T_MASK) | (t))

#define Hd_info(h) ((byte)(((h) & HD_I_MASK) >> 8))
#define Hd_with_info(h, i) (((h) & ~HD_I_MASK) | (header)(i) << 8)

#define Hd_payload(h) ((ui32)(((h) & HD_P_MASK) >> 32))
#define Hd_with_payload(h, p) (((h) & ~HD_P_MASK) | (header)(p) << 32)

// object definitions

typedef ui64 object;

enum object_type {
	TYPE_RAW_DATA	= 1,
	TYPE_TUPLE 		= 2,
	TYPE_THUNK		= 3,

	TYPE_TAG_MIN	= 4,
};

#define Obj_header(o) ((header)(o))
#define Obj_data_ptr(t, o) ((t)(&(o) + 1))

// value definitions

typedef header* value;

#define Is_unboxed(v) ((long)(v) << (VALUE_SIZE - 1))
#define Is_object(v) (! Is_unboxed(v))
#define Val_value(v) ((long)(v) >> 1)
#define Val_object(v) (*(v))

#define Unboxed_value(u) ((value)((long)(u) << 1 | 0x1))
#define Boxed_value(h) ((value)(&(h)))
