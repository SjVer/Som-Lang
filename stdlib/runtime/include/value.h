#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef uint8_t byte;
typedef uint32_t ui32;
typedef uint64_t ui64;
typedef double f64;

/*
	Value type:
		type `value` is either a 8-bit aligned
		pointer to an allocated object or an
		unboxed primitive of less than 64 bits,
		indicated by the 64'th bit (or 32 bits
		in case of a 32-bit architecture)

	Object header structure:
		00-07: tag (1 byte)
		08-15: gc status (1 byte)
		16-30: padding (2 bytes)
		31-63: payload (4 bytes)

    Float objects:
        payload: unused
        following data: 8 byte float

	Raw data objects:
		payload: data size in bytes
		following data: data as array

	Tuple objects:
		payload: element count
		following data: array of values
	
	Closure objects:
		payload: argument count
		following data: function pointer and values

	Tagged objects:
		payload: element count
		following data: array of values
*/

// header definitions
#pragma region

typedef ui64 header;

#define VALUE_SIZE __WORDSIZE
#define HEADER_SIZE 8

#define HD_T_MASK ((1ul << 8) - 1ul)
#define HD_S_MASK (((1ul << 8) - 1ul) << 8)
#define HD_P_MASK (((1ul << 32) - 1ul) << 32)

#define Hd_tag(h) ((byte)((h) & HD_T_MASK))
#define Hd_with_tag(h, t) (((h) & ~HD_T_MASK) | (t))

#define Hd_status(h) ((byte)(((h) & HD_S_MASK) >> 8))
#define Hd_with_status(h, i) (((h) & ~HD_S_MASK) | (header)(i) << 8)

#define Hd_payload(h) ((ui32)(((h) & HD_P_MASK) >> 32))
#define Hd_with_payload(h, p) (((h) & ~HD_P_MASK) | (header)(p) << 32)

#define TAG_MAX 250
#define TAG_FLOAT 251
#define TAG_RAW_DATA 252
#define TAG_RECORD 253
#define TAG_TUPLE 254
#define TAG_CLOSURE 255

#define STATUS_DEAD  0x00
#define STATUS_DYING 0x01
#define STATUS_ALIVE 0x02
#define STATUS_CONST 0x03

#pragma endregion

// object definitions
#pragma region

typedef ui64 object;

#define Obj_header(o) ((header)(o))
#define Obj_data_ptr(t, o) ((t)(&(o) + 1))

#pragma endregion

// value definitions
#pragma region 

typedef header* value;

#define Is_unboxed(v) ((long)(v) << (VALUE_SIZE - 1))
#define Is_object(v) (! Is_unboxed(v))
#define Val_value(v) ((long)(v) >> 1)
#define Val_object(v) (*(v))

#define Unboxed_val(u) ((value)((long)(u) << 1 | 0x1))
#define Boxed_value(h) ((value)(&(h)))

#pragma endregion

// convinient macros
#pragma region

#define Val_data_ptr(v) (v + 1)
#define Val_float(v) (*Obj_data_ptr(f64*, *(v)))
#define Val_field(v, i) (Obj_data_ptr(value*, *(v))[i])
#define Null_val Unboxed_val(0)

#pragma endregion
