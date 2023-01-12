open Parse.Ast
open Report.Error

let ext = Config.extension

let get_ast_fn:
  (string -> Span.t -> ast) ref =
  ref (fun _ _ : ast -> assert false)

(* error helper functions *)

let file_not_found_error path =
  let e = Failed_to_import (path.item ^ ext) in
  Report.make_error (Other_error e) (Some path.span)
  |> Report.add_note (Printf.sprintf
    "try adding directory '%s/' or\n\
    file '%s%s' to the search paths."
    path.item path.item ext)
  |> Report.raise

let failed_to_resolve_error node =
  let e = Failed_to_resolve node.item in
  Report.make_error (Type_error e) (Some node.span)
  |> Report.raise

let mod_has_no_mod_error sname name =
  let e = Has_no_module (sname, name.item) in
  Report.make_error (Type_error e) (Some name.span)
  |> Report.raise

let shadowed_symbol_warning n s =
  let msg = Printf.sprintf "import shadows previous import of `%s`" n in
  Report.make_warning msg (Some s)
  |> Report.report

let check_import_private n =
  if n.item.[0] = '_' then
    let e = Cannot_private ("import", n.item) in
    Report.make_error (Type_error e) (Some n.span)
    |> Report.report

let resolve =
  let _names = ref [] in
  (* execute on each toplevel node *)
  let go (node: toplevel node) =
    let _make_ghost i =
      {
        span={node.span with ghost=true};
        item=i;
      }
    in
    try match node.item with
      (* | TL_Import i ->
        let new_tls = resolve_import names i node.span in
        List.map make_ghost new_tls *)
      | _ -> [node]
    with Report.Error e ->
      Report.report e;
      []
  in
  let rec go' ast = function
    | [] -> ast
    | tl :: tls -> go' (ast @ go tl) tls
  in go' []