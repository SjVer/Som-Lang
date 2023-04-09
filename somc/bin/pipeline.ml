module Ident = Symbols.Ident

module ReadFile = Query.Make(struct
  type a = string * Span.t option
  type r = string
  let c (file, imp_span) =
    try
      let chan = open_in file in
      let len = in_channel_length chan in
      let str = really_input_string chan len in
      close_in chan;
      str
    with _ ->
      let file = file in
      let open Report in
      let open Error in
      make_error (Other_error (Could_not_open file)) imp_span
      |> report;
      Stdlib.exit 1
end)

module ParseFile = Query.Make(struct
  type a = string * Span.t option
  type r = Parse.Ast.ast
  let c (file, imp_span) =
    let source = ReadFile.call (file, imp_span) in
    Parse.parse file source imp_span
end)

module AnalyzeFile = Query.Make(struct
  type a = string * Span.t option
  type r = Parse.Ast.ast
  let c (file, imp_span) =
    (* if [m] ("module") is the ident of
       the module that's importing this one.
       [i] is an optional span of the import
       statement. *)
    let ast = ParseFile.call (file, imp_span) in
    Analysis.resolve ast
end)

module TypecheckFile = Query.Make(struct
  type a = string
  type r = Typing.TAst.tast
  let c file =
    let ast = AnalyzeFile.call (file, None) in
    
    (* we can just manually insert the ast from the prelude
       in the parsed ast because it isn't really imported
       so it doesn't need its imports to be resolved. *)
    let ast' = if (!Configs.Cli.args).no_prelude
      then ast
      else Analysis.add_implicit_prelude ast
    in

    let env = Typing.Env.empty in
    let tast = Typing.typecheck_ast env ast' in
    tast
end)

module LowerFile = Query.Make(struct
  type a = string
  type r = Lambda.Ir.program
  let c file =
    let tast = TypecheckFile.call file in
    Lambda.convert tast
end)

module CodegenFile = Query.Make(struct
  type a = string
  type r = Codegen.llmodule
  let c file =
    let program = LowerFile.call file in
    Codegen.codegen_program program
end)

(* export functions bc otherwise i'd somehow
   have to solve dependency cycles *)
let init () =
  Report.Util.read_file_fn := (fun f -> ReadFile.call (f, None));
  Analysis.Import.get_ast_from_file := fun f s ->
    AnalyzeFile.call (f, Some s)

let () = init ()