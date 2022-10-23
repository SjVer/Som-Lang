module Ast = Ast
module PrintAst = Print_ast
module Ident = Ident

open Report.Error
open Span

let parse file source import_span =
  let lexbuf = Lexing.from_string source in

  try
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
    Parser.prog Lexer.main lexbuf

  with
    | Report.Error e ->
      Report.report e;
      begin match import_span with
        | Some _ ->
          Report.make_error
            (Other_error (Failed_to_import file))
            import_span
          |> Report.report;
          []
        | None ->
          Report.make_error
            (Other_error (Could_not_compile file))
            None
          |> Report.report;
          Report.exit 1
      end
    | Parser.Error ->
      let span = span_from_lexbuf lexbuf false in
      Report.make_error (Syntax_error Unexpected) (Some span)
      |> Report.report;
      Report.exit 1