open Typing.TAst
open Ir

(* from https://github.com/ocaml/ocaml/blob/trunk/lambda/translcore.ml#L191-L194 *)
let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> invalid_arg "lower cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

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

let rec arity_of_type =
  (* TODO: try a less naive approach? *)
  let open Typing.Types in function
    | TFun (_, t) -> 1 + arity_of_type t
    | _ -> 0

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

let rec bind_patt_vars env patt =
  match patt.item with
    | Tpat_wildcard -> env
    | Tpat_variable v ->
      let var = Env.mangle v in
      Env.bind_local env (Ident v) var
    | Tpat_literal _ -> env
    | Tpat_construct (_, args) ->
      List.fold_left bind_patt_vars env args
    | Tpat_tuple patts ->
      List.fold_left bind_patt_vars env patts

and lower_expr env expr =
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

    | Texp_match (scrut, cases) ->
      let scrut' = lower_expr env scrut in
      let cases' = List.map (fun (p, e) ->
        let env = bind_patt_vars env p in
        env, p, lower_expr env e) cases
      in
      Matching.lower_cases env scrut' cases'
    
    | Texp_switch cases ->
      let r = Env.fresh () in
      let scrut = Expr_atom (local r) in
      let cases' = List.map (fun (p, e) ->
        let env = bind_patt_vars env p in
        env, p, lower_expr env e) cases
      in
      let match' = Matching.lower_cases env scrut cases' in
      Expr_lambda ([r], match')

    | Texp_if (cond, texp, eexp) ->
      Expr_if (
        lower_expr env cond,
        lower_expr env texp,
        lower_expr env eexp)

    | Texp_sequence (e1, e2) ->
      let e1' = lower_expr env e1 in
      let e2' = lower_expr env e2 in
      Expr_sequence (e1', e2')

    | Texp_apply ({ item = Texp_primitive p; _ }, args)
      when List.length args >= Symbols.Primitive.arity p ->
       
      let args' = List.map (lower_expr env) args in
      let prim_args, extra_args = cut (Symbols.Primitive.arity p) args' in

      let prim_app = Expr_call (Atom_prim p, prim_args) in
      if extra_args = [] then prim_app
      else Expr_apply (prim_app, extra_args)

    | Texp_apply (f, args) ->
      let f' = lower_expr env f in
      let args' = List.map (lower_expr env) args in
      Expr_apply (f', args')

    | Texp_tuple es ->
      let es' = List.map (lower_expr env) es in
      wrap_exprs_in_vars es' (fun rs -> Expr_tuple rs)
    
    | Texp_construct (ident, args) ->
      let tag = match Env.find env ident.item with
        | Var_tag tag -> tag
        | _ -> failwith "Texp_construct ident no tag"
      in
      let args' = List.map (lower_expr env) args in
      let expr args'' = Expr_object (tag, args'') in
      wrap_exprs_in_vars args' expr

    | Texp_literal l -> Expr_atom (lower_literal l)
    | Texp_ident i -> Expr_atom (Atom_var (Env.find env i.item))      
   
    | Texp_primitive p ->
      begin match p with
        | Prim_file ->
          Expr_atom (Atom_const (Const_string expr.span.file))
        | Prim_line ->
          Expr_atom (Atom_const (Const_int expr.span.start.line))
        | p ->
          (* not directly applied, so wrap in function *)
          let args =
            let rec f i =
              if i = 0 then []
              else Env.mangle "parg" :: f (i - 1)
            in
            f (Symbols.Primitive.arity p)
          in
          let call = Expr_call (
            Atom_prim p,
            List.map (fun a -> Expr_atom (local a)) args)
          in
          Expr_lambda (args, call)
      end

    | Texp_error -> invalid_arg "Pexp_error"

let lower_toplevel env (tl : toplevel node) =
  match tl.item with
    | Ttl_value_def vdef ->
      let var = Env.mangle_ident vdef.vd_name.item in
      let expr = lower_expr env vdef.vd_expr in
      let stmt = Stmt_definition (var, expr) in
      let vars = Env.bind_global env vdef.vd_name.item var in
      vars, Some stmt

    | Ttl_type_def tdef ->
      let open Typing.Types in
      let vars = match tdef.td_type.item with
        | TVariant rows ->
          (* TODO: constr. of other type with same name is overwritten? *)
          let f (vars, tag) (ident, _) =
            if tag > Configs.maximum_tag then begin
              let open Report in
              let e = Error.((Other_error (Other "constructor limit reached"))) in
              make_error e (Some tdef.td_type.span) |> raise
            end;
            Env.add ident (Var_tag tag) vars, tag + 1
          in
          List.fold_left f (env, 1) rows
          |> fst
        | _ -> env
      in
      vars, None

    | Ttl_extern_def edef ->
      let var = Env.mangle_ident edef.ed_name.item in
      let vars = Env.bind_global env edef.ed_name.item var in
      let arity = arity_of_type edef.ed_type.item in
      let stmt = Stmt_external (var, edef.ed_native_name.item, arity) in
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
