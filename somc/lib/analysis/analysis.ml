open Parse.Ast

module Import = Import
module Context = Context
module IMap = Context.IMap

let add_implicit_prelude ast =
  let open Span in
  let open Parse.Ast in
  let open Configs in

  let loc = Loc.{line = 0; col = 0; offset = 0} in
  let span =
    {
      file = prelude_file;
      start = loc;
      end_ = loc;
      ghost = true;
    }
  in
  let node i = {span; item = i} in
  ignore node;

  let mod_ident = Ident.from_list prelude_ident in
  let _, imp_ast = !Import.get_ctx_and_ast prelude_file mod_ident span in
  imp_ast @ ast

let resolve mod_ident (ast : ast) : Context.t * ast =
  Constant_fold.fold_constants ast
  |> Builtins.rename_builtins
  |> Name_resolution.resolve mod_ident