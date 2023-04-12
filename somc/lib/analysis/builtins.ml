open Parse.Ast
open Symbols.Ident

let rename_ident node =
  let renamed, item = match node.item with
    | Ident "::" -> true, from_list ["_std_list"; "Cons"]
    | Ident "[]" -> true, from_list ["_std_list"; "Nil"]
    | _ as i -> false, i
  in
  let ghost = node.span.ghost || renamed in
  {
    span = {node.span with ghost};
    item;
  }

let rec rename_expr e =
  let item = match e.item with
    | Pexp_grouping e -> Pexp_grouping (rename_expr e)
    | Pexp_binding (b, e) ->
      let b' = {b with vb_expr = rename_expr b.vb_expr} in
      let e' = rename_expr e in
      Pexp_binding (b', e')
    | Pexp_lambda {vb_patt; vb_expr} ->
      let b' = {vb_patt;  vb_expr = rename_expr vb_expr} in
      Pexp_lambda b'
    | Pexp_sequence (e1, e2) ->
      Pexp_sequence (rename_expr e1, rename_expr e2)
    | Pexp_constraint (e, t) -> Pexp_constraint (rename_expr e, t)
    | Pexp_apply (f, es) ->
      let es' = List.map rename_expr es in
      Pexp_apply (rename_expr f, es')
    | Pexp_tuple es -> Pexp_tuple (List.map rename_expr es)
    | Pexp_construct (i, es) ->
      Pexp_construct (rename_ident i, List.map rename_expr es)
    | Pexp_ident i -> Pexp_ident (rename_ident i)
    | _ -> e.item
  in
  {e with item}

let rec rename_builtins (ast : ast) : ast =
  let go tl =
    let item = match tl.item with
      | Ptl_value_def {vd_name; vd_expr} ->
        Ptl_value_def {vd_name; vd_expr = rename_expr vd_expr}
      | Ptl_module (n, ast) ->
        Ptl_module (n, rename_builtins ast)
      | _ as tl -> tl
    in
    {tl with item}
  in
  List.map go ast