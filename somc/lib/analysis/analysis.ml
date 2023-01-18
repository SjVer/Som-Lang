open Parse.Ast

module Name_resolution = Name_resolution

type ast_symbol_table = Scope.ast_symbol_table
let print_ast_table = Scope.print_ast_table

let add_implicit_prelude ast =
  let open Span in
  let open Parse.Ast in
  let open Config in

  let loc = Loc.{line=0; col=0; offset=0} in
  let span = {file=prelude_file; start=loc; end_=loc; ghost=true} in
  let node i = {span; item=i} in

  let _import =
    {
      i_path = List.map node prelude_import_path;
      i_kind = node IK_Glob;
    }
  in

  let tls =
    (* try Import.resolve_import (ref []) import span
    with Report.Error r -> Report.report r; [] *)
    []
  in
  List.map node tls @ ast

let resolve mod_ident is_import (ast : ast) : ast_symbol_table =
  Constant_fold.fold_constants ast |>
  Builtins.rename_builtins |>
  Name_resolution.resolve mod_ident is_import
