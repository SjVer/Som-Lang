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
      let open Report.Error in
      let e = Other_error (Could_not_open file) in
      Report.Error.raise_error e None []
end)

module ParseFile = Query.Make(struct
  type a = string
  type r = Parse.Ast.ast
  let c f =
    let source = ReadFile.call f in
    Parse.parse f source
end)

module RefineFile = Query.Make(struct
  type a = string
  type r = Parse.Ast.ast
  let c f =
    let ast = ParseFile.call f in
    Refine.desugar ast
end)

module TypecheckFile = Query.Make(struct
  type a = string
  type r = Typing.TAst.tast
  let c f =
    let ast = RefineFile.call f in
    let _, tast = Typing.typecheck Typing.Env.empty ast in
    tast
end)

(* export functions bc otherwise i'd somehow
   have to solve dependency cycles *)
let _ =
  Report.Util.read_file_fn := ReadFile.call;
  Refine.Import.get_ast_fn := RefineFile.call;