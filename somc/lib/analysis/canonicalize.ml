open Parse.Ast
open Symboltable
open Ident

let mk_g n i = {span = {n.span with ghost = true}; item = i}

let canon_name m name = Cons (m, name)
let canon_ident m ident = Ident.prepend m ident
let canon_ident' m inode = mk_g inode (canon_ident m inode.item)

let rec canon_type m typ =
  let go = canon_type m in
  let item = match typ.item with
    (* | TY_Variant vs ->
      let vs' = List.map (fun (n, ts) -> (canon_name' n, ts)) vs in
      TY_Variant vs' *)
    | TY_Grouping t -> TY_Grouping (go t)
    | TY_Forall (ps, t) -> TY_Forall (ps, go t)
    | TY_Effect t -> TY_Effect (go t) 
    | TY_Function (t1, t2) -> TY_Function (go t1, go t2)
    | TY_Tuple ts -> TY_Tuple (List.map (go) ts)
    | TY_Construct (t, i) -> TY_Construct (Option.map go t, canon_ident' m  i)
    | item -> item
  in
  {typ with item}

let rec canon_expr m expr =
  let go = canon_expr m in
  let item = match expr.item with
    | EX_Grouping e -> EX_Grouping (go e)
    (* TODO: canon patt? *)
    | EX_Binding (b, e) -> EX_Binding ({b with vb_expr = go b.vb_expr}, go e)
    | EX_Lambda b -> EX_Lambda {b with vb_expr = go b.vb_expr}
    | EX_Sequence (e1, e2) -> EX_Sequence (go e1, go e2)
    | EX_Constraint (e, t) -> EX_Constraint (go e, canon_type m t)
    | EX_Application (e, es) -> EX_Application (go e, List.map go es)
    | EX_Tuple es -> EX_Tuple (List.map go es)
    | EX_Construct (i, es) -> EX_Construct (canon_ident' m i, List.map go es)
    | EX_Identifier i -> EX_Identifier (canon_ident' m i)
    | item -> item
  in
  {expr with item}

let rec canon_toplevel m t tl =
  match tl.item with
    | TL_Value_Definition vdef ->
      let ident = canon_name m vdef.vd_name.item in
      let vdef' = {vdef with vd_expr = canon_expr m vdef.vd_expr} in
      add_new_value t ident vdef'
    | TL_Type_Definition tdef ->
      let ident = canon_name m tdef.td_name.item in
      let tdef' = {tdef with td_type = canon_type m tdef.td_type} in
      add_new_type t ident tdef'
    | TL_Module (n, ast) ->
      let ident = Ident n.item in
      List.fold_left (canon_toplevel ident) t ast
    | _ -> t

let canonicalize_ast m ast =
  List.fold_left (canon_toplevel m) Symboltable.empty ast