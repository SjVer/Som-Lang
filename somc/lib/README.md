# Libraries

## analysis/

Responsible for name resolution.

## codegen/

Responsible for translating ??? into LLVM IR and generating the final binary.

## config/

Contains configurations

## parse/

Responsible for opening, lexing and parsing source files into an AST.

## pipeline/

Contains the compiler queries.

## report/

Responsible for generating and printing/reporting diagnostics.
It also handles anything error-code related.

## span/

Contains the `span` and `loc` types and code related to those types.

## typing/

Responsible for typechecking an AST into a TAST.
It also resolves/handels imports and does a bit of desugaring.
