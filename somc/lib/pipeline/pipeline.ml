module ReadFile = Query.Make(struct
  type a = string * Span.t option
  type r = string
  let c (f, s) =
    try
      let chan = open_in f in
      let len = in_channel_length chan in
      let str = really_input_string chan len in
      close_in chan;
      str
    with _ ->
      let file = f in
      let open Report in
      let open Error in
      make_error (Other_error (Could_not_open file)) s
      |> report;
      Report.exit 0
end)

module ParseFile = Query.Make(struct
  type a = string * Span.t option
  type r = Parse.Ast.ast
  let c (f, s) =
    let source = ReadFile.call (f, s) in
    Parse.parse f source s
end)

module AnalyzeFile = Query.Make(struct
  type a = string * Span.t option
  type r = Analysis.ast_symbol_table
  let c (f, i) =
    let ast = ParseFile.call (f, i) in
    let mod_name = Filename.(chop_extension (basename f)) in
    Analysis.resolve mod_name ast
end)

module TypecheckFile = Query.Make(struct
  type a = string
  type r = Typing.TAst.tast
  let c f =
    let _table = AnalyzeFile.call (f, None) in
    (* let table' = if (!Config.Cli.args).no_prelude
      then table
      else Analysis.add_implicit_prelude table
    in
    let env = Typing.Env.empty () in
    let tast = Typing.typecheck env table' in
    tast
    *)
    []
end)

(* export functions bc otherwise i'd somehow
   have to solve dependency cycles *)
let init () =
  Report.Util.read_file_fn := (fun f -> ReadFile.call (f, None));
  Analysis.Name_resolution.get_ast_symbol_table := fun f s ->
    AnalyzeFile.call (f, Some s)

let () = init ()