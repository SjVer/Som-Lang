open Parse.Ast
open Symboltable
open Ident
open Scope

let mk_g n i = {span = {n.span with ghost = true}; item = i}

let canon_name s name = Ident.append_opt s.name (Ident name)
let canon_ident s ident = Ident.append_opt s.name ident
let canon_ident' s inode = mk_g inode (canon_ident s inode.item)

let rec canon_type s typ =
  let go = canon_type s in
  let item = match typ.item with
    (* | TY_Variant vs ->
      let vs' = List.map (fun (n, ts) -> (canon_name' n, ts)) vs in
      TY_Variant vs' *)
    | TY_Grouping t -> TY_Grouping (go t)
    | TY_Forall (ps, t) -> TY_Forall (ps, go t)
    | TY_Effect t -> TY_Effect (go t) 
    | TY_Function (t1, t2) -> TY_Function (go t1, go t2)
    | TY_Tuple ts -> TY_Tuple (List.map (go) ts)

    | TY_Construct (t, i) -> begin try
        let i' = Scope.lookup_type_or_error s i.item i.span in
        TY_Construct (Option.map go t, {i with item = i'})
      with Not_found -> TY_Any end

    | item -> item
  in
  {typ with item}

let rec canon_expr s expr =
  let go = canon_expr s in
  let item = match expr.item with
    | EX_Grouping e -> EX_Grouping (go e)
    (* TODO: canon patt? *)
    | EX_Binding (b, e) -> EX_Binding ({b with vb_expr = go b.vb_expr}, go e)
    | EX_Lambda b -> EX_Lambda {b with vb_expr = go b.vb_expr}
    | EX_Sequence (e1, e2) -> EX_Sequence (go e1, go e2)
    | EX_Constraint (e, t) -> EX_Constraint (go e, canon_type s t)
    | EX_Application (e, es) -> EX_Application (go e, List.map go es)
    | EX_Tuple es -> EX_Tuple (List.map go es)

    | EX_Construct (i, es) -> begin try
        let i' = Scope.lookup_value_or_error s i.item i.span in
        EX_Construct ({i with item = i'}, List.map go es)
      with Not_found -> EX_Error end  
    | EX_Identifier i -> begin try
        let i' = Scope.lookup_value_or_error s i.item i.span in
        EX_Identifier {i with item = i'}
      with Not_found -> EX_Error end  
    | item -> item
  in
  {expr with item}

let rec canon_toplevel s tl : scope =
  match tl.item with
    | TL_Value_Definition vdef ->
      let ident = Ident vdef.vd_name.item in
      let vdef' = {vdef with vd_expr = canon_expr s vdef.vd_expr} in
      Scope.add_new_value s ident (canon_ident s ident) vdef'
    | TL_Type_Definition tdef ->
      let ident = Ident tdef.td_name.item in
      let tdef' = {tdef with td_type = canon_type s tdef.td_type} in
      Scope.add_new_type s ident ident tdef'
    | TL_Module (n, ast) ->
      let subscope = canon_ast (Scope.push s n.item) ast in
      Scope.add_table s subscope.table
    | _ -> s

and canon_ast s ast =
  List.fold_left canon_toplevel s ast

let canon_table mod_name table =
  let scope = Scope.empty mod_name in
  let value_fn k e map =
    let vd_expr = canon_expr scope e.symbol.vd_expr in
    let symbol = {e.symbol with vd_expr} in
    IMap.add (canon_ident scope k) {e with symbol} map
  in
  let type_fn k e map =
    let td_type = canon_type scope e.symbol.td_type in
    let symbol = {e.symbol with td_type} in
    IMap.add (canon_ident scope k) {e with symbol} map
  in

  let values = IMap.fold value_fn table.values IMap.empty in
  let types = IMap.fold type_fn table.types IMap.empty in
  {values; types}