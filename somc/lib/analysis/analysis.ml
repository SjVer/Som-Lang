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
  let ast = Constant_fold.fold_constants ast in
  let ctx =  Builtins.bind_builtins (Context.empty mod_ident) in

  (* we keep all imported ast nodes seperate for now
     so that they don't get messed up by `resolve_ast` *)
  let ctx, imp_ast, main_ast = Import.gather_and_apply_imports ctx ast in

  (* we have the bindings of the imports in `ctx'` so we can
     just pretend everything is in place and resolve this ast *)
  let ctx, main_ast = Resolve.resolve_ast ctx main_ast in

  (* finally we do prepend the imported ast with this ast *)
  ctx, imp_ast @ main_ast