open Typing.TAst
open Ir

(* variables stuff *)

module IMap = Map.Make(Ident)

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

let bind_global vars ident var = IMap.add ident (Var_global var) vars
let bind_local vars ident var = IMap.add ident (Var_local var) vars

let mangle =
  let c = ref 0 in
  fun str -> begin
    incr c;
    str ^ "/" ^ string_of_int !c
  end

let mangle_ident i = Ident.to_string i |> mangle

let fresh () = mangle "r"

let local r = Atom_var (Var_local r)

let wrap_exprs_in_vars exprs cont =
  let rs = List.map (fun _ -> fresh ()) exprs in
  let f acc (r, el) = Expr_let (r, el, acc) in

  List.fold_left f
    (cont (List.map local rs))
    (List.combine rs exprs |> List.rev)

(* let make_lazy = function
  | Expr_lambda _ as e -> e
  | e -> Expr_lazy e *)

(* lowering stuff *)

let lower_literal l =
  let const = match l with
    | LIInt i -> Const_int i
    | LIChar c -> Const_int (Char.code c)
    | LIFloat f -> Const_float f
    | LIString s -> Const_string s
    | LINil -> Const_nil
  in
  Atom_const const

let lower_pattern vars patt =
  match patt.item with
    | PAVariable i ->
      let var = mangle i in
      bind_local vars (Ident i) var, [var]
    | PAWildcard ->
      vars, [mangle "_"]

let lower_application f args =
  let app args' =
    let r = fresh () in
    let app = Expr_apply (Expr_atom (local r), args') in
    Expr_let (r, f, app)
  in
  wrap_exprs_in_vars args app

let rec lower_expr vars expr =
  match expr.item with
    | EXGrouping e -> lower_expr vars e

    | EXBinding _ -> failwith "lower EXBinding"
    (* | EXBinding (bind, body) ->
      let vars, params = lower_pattern vars bind.vb_patt in
      let e = lower_expr cont_atom vars bind.vb_expr in
      let bound = mangle "TODO" in
      let body = lower_expr cont_atom vars body in
      Expr_lambda (bound, (params, e), body) *)

    | EXLambda bind ->
      let vars, params = lower_pattern vars bind.vb_patt in
      let e = lower_expr vars bind.vb_expr in
      Expr_lambda (params, e)

    | EXSequence (e1, e2) ->
      let e1' = lower_expr vars e1 in
      let e2' = lower_expr vars e2 in
      Expr_sequence (e1', e2')

    | EXApplication (f, args) ->
      let f' = lower_expr vars f in
      let args' = List.map (lower_expr vars) args in
      lower_application f' args'

    | EXTuple es ->
      let es' = List.map (lower_expr vars) es in
      wrap_exprs_in_vars es' (fun rs -> Expr_tuple rs)
    
    | EXConstruct (ident, args) ->
      let tag = match IMap.find ident.item vars with
        | Var_tag tag -> tag
        | _ -> failwith "EX_Constructor ident no tag"
      in
      let args' = List.map (lower_expr vars) args in
      let expr args'' = Expr_object (tag, args'') in
      wrap_exprs_in_vars args' expr

    | EXLiteral l -> Expr_atom (lower_literal l)
    | EXIdentifier i -> Expr_atom (Atom_var (IMap.find i.item vars))      
    | EXMagical m -> Expr_atom (Atom_magic m)

    | EXError -> invalid_arg "EXError"

let lower_toplevel vars (tl : toplevel node) =
  match tl.item with
    | TLValueDef vdef ->
      let var = mangle_ident vdef.vd_name.item in
      let expr = lower_expr vars vdef.vd_expr in
      let stmt = Stmt_definition (var, expr) in
      let vars = bind_global vars vdef.vd_name.item var in
      vars, Some stmt

    | TLTypeDef tdef ->
      let open Typing.Types in
      let vars = match tdef.td_type.item with
        | TVariant rows ->
          let f (vars, tag) (ident, _) =
            IMap.add ident (Var_tag tag) vars, tag + 1
          in
          List.fold_left f (vars, Configs.minimum_tag) rows
          |> fst
        | _ -> vars
      in
      vars, None

    | TLExternDef edef ->
      let var = mangle_ident edef.ed_name.item in
      let stmt = Stmt_external (var, edef.ed_native_name.item) in
      let vars = bind_global vars edef.ed_name.item var in
      vars, Some stmt

let lower_tast tast =
  let rec go vars program = function
    | [] -> program
    | tl :: tast ->
      let vars, stmt = lower_toplevel vars tl in
      go vars (program @ [stmt]) tast
  in
  go IMap.empty [] tast
  |> List.filter_map (fun x -> x)
