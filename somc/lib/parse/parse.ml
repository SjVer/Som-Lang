module Ast = Ast
module PrintAst = Print_ast
module Span = Span

let string_from_position (lexbuf: Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let fname = if String.length pos.pos_fname > 0 then pos.pos_fname else "<file>" in
  Printf.sprintf "%s:%d:%d" fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse file =
  let lexbuf = Lexing.from_channel (open_in file) in
  try Parser.prog Lexer.main lexbuf with
  (* | Lexer.SyntaxError msg -> fprintf stderr "%s: %s\n" (string_from_position lex_buf) msg; exit 1 *)
  | Parser.Error -> Printf.fprintf stderr "%s: Syntax error\n" (string_from_position lexbuf); exit 1
