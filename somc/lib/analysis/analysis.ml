module Import = Import

let add_implicit_prelude ast =
  let open Span in
  let open Parse.Ast in
  let open Config in

  let loc = Loc.{line=0; col=0; offset=0} in
  let span = {file=prelude_file; start=loc; end_=loc; ghost=true} in
  let node i = {span; item=i} in

  let _import =
    {
      path=List.map node prelude_import_path;
      kind=node IK_Glob;
    }
  in

  let tls =
    (* try Import.resolve_import (ref []) import span
    with Report.Error r -> Report.report r; [] *)
    []
  in
  List.map node tls @ ast

let check ast =
  Import.resolve ast |>
  Constant_fold.fold_constants |>
  Builtins.rename_builtins
