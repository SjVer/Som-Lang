open Parse.Ast

module Import = Import
module Context = Context
module IMap = Context.IMap

let add_implicit_prelude ast =
  let span = Span.dummy Configs.prelude_file in
  let imp_ast = !Import.get_ast_from_file Configs.prelude_file span in
  imp_ast @ ast

let resolve ast : ast =
  let init_ctx = Context.empty (Ident "") in
  ast
  |> Import.include_imports
  |> Resolve.resolve_ast init_ctx |> snd
  |> Constant_fold.fold_constants
