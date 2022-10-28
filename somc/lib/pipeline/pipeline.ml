module ReadFile = Query.Make(struct
  type a = string
  type r = string
  let c f =
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
      make_error (Other_error (Could_not_open file)) None
      |> raise
end)

module ParseFile = Query.Make(struct
  type a = string * Span.t option
  type r = Parse.Ast.ast
  let c (f, i) =
    let source = ReadFile.call f in
    Parse.parse f source i
end)

module AnalyzeFile = Query.Make(struct
  type a = string * Span.t option
  type r = Parse.Ast.ast
  let c (f, i) =
    let ast = ParseFile.call (f, i) in
    Analysis.check ast
end)

module TypecheckFile = Query.Make(struct
  type a = string
  type r = Typing.TAst.tast
  let c f =
    let ast = AnalyzeFile.call (f, None) in
    let ast' = if (!Config.Cli.args).no_prelude
      then ast
      else Analysis.add_implicit_prelude ast
    in
    let _, tast = Typing.typecheck Typing.Env.empty ast' in
    tast
end)

(* export functions bc otherwise i'd somehow
   have to solve dependency cycles *)
let init () =
  Report.Util.read_file_fn := ReadFile.call;
  Analysis.Import.get_ast_fn := fun f s ->
    AnalyzeFile.call (f, (Some s))

let () = init ()