#pragma once

#include <bits/types.h>

typedef __uint8_t byte;
typedef __uint32_t ui32;
typedef __uint64_t ui64;

typedef _Bool		prim_iu1;
typedef _Bool		prim_is1;

typedef __uint8_t	prim_iu8;
typedef __int8_t	prim_is8;

typedef __uint16_t	prim_iu16;
typedef __int16_t	prim_is16;

typedef __uint32_t  prim_iu32;
typedef __int32_t   prim_is32;

typedef __uint64_t  prim_iu64;
typedef __int64_t   prim_is64;

typedef __uint128_t	prim_iu128;
typedef __int128_t  prim_is128;

#if __WORDSIZE == 64
typedef __uint64_t	prim_ius;
typedef __int64_t	prim_iss;
#elif __WORDSIZE == 32
typedef __uint32_t	prim_ius;
typedef __int32_t	prim_iss;
#else
#error "incompatible __WORDSIZE"
#endif

typedef __fp16		prim_f16;
typedef float		prim_f32;
typedef double		prim_f64;

typedef _Bool     	prim_v;
