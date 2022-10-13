module Name_res = Name_res

let add_implicit_import_prelude ast =
  let open Span in
  let open Parse.Ast in
  let open Config in

  let loc = Loc.{line=0; col=0; offset=0} in
  let span = {file=prelude_file; start=loc; end_=loc; ghost=true} in
  let node i = {span; item=i} in

  let import =
    {
      dir=[prelude_dir];
      path=[node prelude_ident];
      kind=node IK_Glob;
    }
  in
  let tls = Name_res.resolve_import (ref []) import span in
  List.map node tls @ ast

let added_implicit_import = ref false

let check ast =
  let ast' = Name_res.resolve ast in

  if not !added_implicit_import then begin
    added_implicit_import := true;
    add_implicit_import_prelude ast'
  end else ast'