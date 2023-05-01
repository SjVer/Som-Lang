# Hacking on the runtime

## Naming

**Standard libary implementations:**
Implementations of functions exposed to Som should be named
with the `som_` prefix. These symbols automatically get renamed
to `_som_<path>_<name>` during the build process.

**Internal public symbols:**
Symbols used internally in multiple files should be named
with the `_som_` prefix. These symbols keep their names.

**Internal private symbols:**
Symbols used internally in only one file should be named
with the `s_` prefix. These symbols automatically get renamed
to `som_<path>___<name>` during the build process.

## Tags

Constructor/variant tags are assigned in order of declaration
starting with 0 by the Som compiler. Therefore, their tags
should be implicitly defined using an enum in the runtime.
The tags should then automatically be correct.
