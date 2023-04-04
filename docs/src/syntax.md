# The Complete Syntax

Here is the complete syntax of Som in a simplified BNF. Do note that it might be incomplete due to the continuing development of the Som language. The irony of this has been duly noted.

```python
program            = toplevel*

toplevel           = import_statement
                   | module_definition
                   | value_definition
                   | type_definition
                   | extern_definition

import_statement   = "use" LONG_LIDENT
                   | "from" LONG_LIDENT "use" "*"
                   | "from" LONG_LIDENT "use" (LONG_IDENT ",")* LONG_IDENT

module_definition  = "mod" "{" toplevel* "}"

value_definition   = "let" LIDENT pattern* "=" expression

type_definition    = "type" PIDENT* UIDENT "is" type
                   | "type" PIDENT* UIDENT "of" complex_type
                   
extern_definition  = "ext" LIDENT ":" type
                   | "ext" LIDENT "as" LIDENT pattern* ":" type

complex_type       = "|"? variant ("|" variant)*
                   | "{" (field ("," field)*)? "}"
                   
variant            = UIDENT (type (";" type)*)?
field              = LIDENT ":" type

type               = forall_type
forall_type        = (PIDENT+ ".")? function_type
function_type      = (function_type "->")? tuple_type
tuple_type         = constructed_type (";" constructed_type)*
constructed_type   = effect_type LONG_UIDENT?
effect_type        = "!"* atom_type
atom_type          = "(" type ")"
                   | PRIMITIVE_TYPE
                   | PIDENT
                   | LONG_UIDENT

pattern            = atom_pattern
atom_pattern       = LIDENT
                   | "_"

expression         = let_epxr
                   | sequence_expr
let_epxr           = "let" LIDENT pattern* "=" let_epxr "in" let_epxr
sequence_expr      = (sequence_expr ",")? lambda_expr
lambda_expr        = ("\\" pattern* "->") tuple_expr
tuple_expr         = contraint_expr (";" constraint_expr)*
constraint_expr    = BASIC_EXPR (":" type)?
```

The meaning of the tokens such as `LIDENT` are as follows:

| token | meaning |
| --- | --- |
| IDENT | Any identifier, such as `foo`, `Bar` or `baz'` |
| LIDENT | A lowercase identifier, such as `foo` |
| UIDENT | An uppercase identifier, such as `Bar` |
| PIDENT | A lowercase identifier starting with a `'`, such as `'var` |
| LONG_* | The qualified version of the identifier, such as `foo::Bar` |
| PRIMITIVE_TYPE | A primitive type, such as `$i.s.32` or `$f.*` |
| BASIC_EXPR | A simple expression, such as `1 + 2` or `"foo"` | 
