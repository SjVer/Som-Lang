open Parse.Ast

module Import = Import
module Context = Context
module IMap = Context.IMap

let add_implicit_prelude ast =
  let span = Span.dummy Configs.prelude_file in
  let imp_ast = !Import.get_ast_from_file Configs.prelude_file span in
  imp_ast @ ast

let initial_ctx = 
  let open Context in
  let bind name ctx = bind_type ctx
    (Ident name) (Ident name)
  in 
  empty (Ident "")
  |> bind "Int"
  |> bind "Chr"
  |> bind "Flt"
  |> bind "Str"
  |> bind "Nil"

let resolve ast : ast =
  ast
  |> Import.include_imports
  |> Resolve.resolve_ast initial_ctx |> snd
  |> Constant_fold.fold_constants
  |> Builtins.rename_builtins