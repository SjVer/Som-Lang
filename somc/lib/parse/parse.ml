module Ast = Ast
module PrintAst = Print_ast
module Ident = Ident

open Report.Error
open Span

let parse file =
  (* lexbuf stuff temporary *)
  let lexbuf = Lexing.from_channel (open_in file) in
  try
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
    Parser.prog Lexer.main lexbuf
  with
  | Error (e, s, n) ->
    Report.report e s n;
    Report.report (Other_error (Could_not_compile file)) None [];
    exit 1
  | Parser.Error ->
    let span = span_from_lexbuf lexbuf false in
    Report.report (Syntax_error Unexpected) (Some span) [];
    exit 1