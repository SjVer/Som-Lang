module Ast = Ast
module Print_ast = Print_ast

open Report.Error

let parse file source import_span =
  let parsing_failed () =
    if Option.is_some import_span then begin
      Report.make_error
        (Other_error (Failed_to_import file))
        import_span
      |> Report.report
    end
  in

  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
  let tokens = Lexer.get_tokens lexbuf in

  let parser =
    Parser.{
      tokens = tokens;
      previous = List.hd tokens;
    }
  in

  let ast = Grammar.parse_file parser in
  if !Parser.Handling.had_error then parsing_failed ();
  ast