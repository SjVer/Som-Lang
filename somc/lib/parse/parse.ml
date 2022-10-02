module Ast = Ast
module PrintAst = Print_ast
module Ident = Ident

open Parser.MenhirInterpreter
open Report.Error
open Span

let succeed node = node

let fail lexbuf (_ : 'a checkpoint) =
  let span = span_from_lexbuf lexbuf false in
  Report.report (Syntax_error Unexpected) (Some span) [];
  exit 1

let loop lexbuf result =
  let supplier = lexer_lexbuf_to_supplier Lexer.main lexbuf in
  loop_handle succeed (fail lexbuf) supplier result

let parse file =
  (* lexbuf stuff temporary *)
  let lexbuf = Lexing.from_channel (open_in file) in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
  loop lexbuf (Parser.Incremental.prog lexbuf.lex_curr_p)