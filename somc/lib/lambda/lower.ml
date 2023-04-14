open Typing.TAst
open Ir

let rec is_func_ty =
  let open Typing.Types in
  function
    | TFun _ -> true
    | TVar {contents = VRSolved ty} -> is_func_ty ty
    | TVar ({contents = VRUnbound (_, level)} as tvar) ->
      (* TODO: remove this? *)
      let param_ty = new_var level in
      let return_ty = new_var level in
      tvar := VRSolved (TFun (param_ty, return_ty));
      true
    | _ -> false

let local r = Atom_var (Var_local r)

let wrap_exprs_in_vars exprs cont =
  let rs = List.map (fun _ -> Env.fresh ()) exprs in
  let f acc (r, el) = Expr_let (r, el, acc) in

  List.fold_left f
    (cont (List.map local rs))
    (List.combine rs exprs |> List.rev)

(* let make_lazy = function
  | Expr_lambda _ as e -> e
  | e -> Expr_lazy e *)

(* lowering stuff *)

let lower_literal l =
  Atom_const (Matching.lower_literal l)

let lower_apply f args =
  let app args' =
    let r = Env.fresh () in
    let app = Expr_apply (Expr_atom (local r), args') in
    Expr_let (r, f, app)
  in
  wrap_exprs_in_vars args app

let rec bind_patt_vars env patt =
  match patt.item with
    | Tpat_wildcard -> env
    | Tpat_variable v ->
      let var = Env.mangle v in
      Env.bind_local env (Ident v) var
    | Tpat_literal _ -> env
    | Tpat_tuple patts ->
      List.fold_left bind_patt_vars env patts

let rec lower_expr env expr =
  match expr.item with
    | Texp_grouping e -> lower_expr env e

    | Texp_binding (bind, expr) ->
      let value = lower_expr env bind.vb_expr in
      let env = bind_patt_vars env bind.vb_patt in
      let expr = lower_expr env expr in
      Matching.lower_binding env bind.vb_patt value expr

    | Texp_lambda bind ->
      let env = bind_patt_vars env bind.vb_patt in
      let body = lower_expr env bind.vb_expr in
      Matching.lower_lambda env bind.vb_patt body

    | Texp_sequence (e1, e2) ->
      let e1' = lower_expr env e1 in
      let e2' = lower_expr env e2 in
      Expr_sequence (e1', e2')

    | Texp_apply (f, args) ->
      let f' = lower_expr env f in
      let args' = List.map (lower_expr env) args in
      lower_apply f' args'

    | Texp_tuple es ->
      let es' = List.map (lower_expr env) es in
      wrap_exprs_in_vars es' (fun rs -> Expr_tuple rs)
    
    | Texp_construct (ident, args) ->
      let tag = match Env.find env ident.item with
        | Var_tag tag -> tag
        | _ -> failwith "EX_Constructor ident no tag"
      in
      let args' = List.map (lower_expr env) args in
      let expr args'' = Expr_object (tag, args'') in
      wrap_exprs_in_vars args' expr

    | Texp_literal l -> Expr_atom (lower_literal l)
    | Texp_ident i -> Expr_atom (Atom_var (Env.find env i.item))      
    | Texp_magic m -> Expr_atom (Atom_magic m)

    | Texp_error -> invalid_arg "Pexp_error"

let lower_toplevel vars (tl : toplevel node) =
  match tl.item with
    | Ttl_value_def vdef ->
      let var = Env.mangle_ident vdef.vd_name.item in
      let expr = lower_expr vars vdef.vd_expr in
      let stmt = Stmt_definition (var, expr) in
      let vars = Env.bind_global vars vdef.vd_name.item var in
      vars, Some stmt

    | Ttl_type_def tdef ->
      let open Typing.Types in
      let vars = match tdef.td_type.item with
        | TVariant rows ->
          let f (vars, tag) (ident, _) =
            Env.add ident (Var_tag tag) vars, tag + 1
          in
          List.fold_left f (vars, Configs.minimum_tag) rows
          |> fst
        | _ -> vars
      in
      vars, None

    | Ttl_extern_def edef ->
      let var = Env.mangle_ident edef.ed_name.item in
      let stmt = Stmt_external (var, edef.ed_native_name.item) in
      let vars = Env.bind_global vars edef.ed_name.item var in
      vars, Some stmt

let lower_tast tast =
  let rec go vars program = function
    | [] -> program
    | tl :: tast ->
      let vars, stmt = lower_toplevel vars tl in
      go vars (program @ [stmt]) tast
  in
  go Env.empty [] tast
  |> List.filter_map (fun x -> x)
