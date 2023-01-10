module Ast = Ast
module PrintAst = Print_ast
module Ident = Ident

open Report.Error

let print_tokens =
  let open Token in
  List.iter (fun t ->
    print_string (Span.show_span t.span);
    print_string ": ";
    print_endline (show_token_typ t.typ)
  )

let parse file source import_span =
  let lexbuf = Lexing.from_string source in

  try
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
    let tokens = Lexer.get_tokens lexbuf in
    (* print_tokens tokens; *)

    Grammar.parse_file Parser.{
      tokens = tokens;
      previous = List.hd tokens;
    }

  with Report.Error e ->
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