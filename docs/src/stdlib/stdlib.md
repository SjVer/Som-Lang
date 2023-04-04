# The Standard Library

Som's standard library provides modules with values and types core to Som, both implemented natively or in C. Some of these modules provide functionality for accessing services "outside" of Som, such as I/O, while other modules implement trivial types such as `List` and `Option`.

Currently, Som's standard contains the following modules:

- [`io`](io.md) for file I/O
- `list` for the list type and related functions
- `ops` for aliasing builtin operators
- [`prelude`](prelude.md) for including basic features
- `ptr` for the pointer type and related functions
- `str` for the string type and related functions
- `types` for aliasing common primitive types

The standard library's runtime implementation is mostly in C and is distributed in the dynamic library `libsom.so` on Linux and MacOS and in `som.dll` on Windows. When using the standard library the final executable has to be linked to this library, which is [normally](../compiler/compilation.md#linking) done automatically by the compiler. Any library or program extending- or using Som's standard library outside of Som has to link to the dynamic library manually.