# Parsing

[[parse.ml]] provides the `parse` function which takes a filename and produces an AST.

[[lexer.mll]] is an ocamllex lexer that lexes source code into tokens.

[[parser.mly]] is the menhir parser that parses a given lexbuf into an AST.

[[ast.ml]] contains the AST types.

[[print_ast.ml]] contains code for pretty-printing AST's.

[[ident.ml]] contains the `Ident.t` type which represents parsed identifiers such as `X` or `Foo::Bar`.
