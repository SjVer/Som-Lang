module Ast = Ast
module PrintAst = Print_ast
module Ident = Ident

open Report.Error
open Span

let parse file source is_import =
  let lexbuf = Lexing.from_string source in

  try
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
    Parser.prog Lexer.main lexbuf

  with
    | Error e ->
      Report.report e;
      if is_import then begin
        Report.report (simple (Other_error (Failed_to_import file)));
        []
      end else begin
        Report.report (simple (Other_error (Could_not_compile file)));
        exit 1
      end
    | Parser.Error ->
      let span = span_from_lexbuf lexbuf false in
      Report.report {
        error=Syntax_error Unexpected;
        span=Some span;
        notes=[];
      };
      exit 1