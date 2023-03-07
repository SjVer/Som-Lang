# Libraries

## analysis/

Responsible for import-, name- and builtin resolution and constant folding.

## codegen/

Contains the Lambda to LLVM translation and backend.

## config/

Contains static- and user-defined (command-line) configurations.

## malfunction/

Contains the Lambda to Malfunction translation and backend.

## parse/

Responsible for opening, lexing and parsing source files into an AST.

## pipeline/

Contains the compiler queries.

## report/

Responsible for generating and printing/reporting diagnostics.
It also handles anything error-code related.

## span/

Contains the `span` and `loc` types and code related to those types.

## symbols/

Contains the definitions for symbols, identifiers, etc...

## typing/

Responsible for typechecking an AST into a TAST.
It also resolves/handels imports and does a bit of desugaring.
