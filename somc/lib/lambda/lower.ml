open Typing.TAst
open Ir

(* variables stuff *)

module IMap = Map.Make(Ident)

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

let cont_atom a = Expr_atom a
let cont_eval = function
  | Atom_const _ as a -> Expr_atom a
  | Atom_var v ->
    let r = fresh () in
    Expr_eval (r, v, cont_atom (local r))

let get_atom = function
  | Expr_atom a -> a
  | _ -> invalid_arg "get_atom"

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
      vars, []

let rec lower_expr cont vars expr =
  match expr.item with
    | EXGrouping e -> lower_expr cont vars e

    | EXBinding (bind, body) ->
      let vars, params = lower_pattern vars bind.vb_patt in
      let e = lower_expr cont_atom vars bind.vb_expr in
      let bound = mangle "TODO" in
      let body = lower_expr cont_atom vars body in
      Expr_lambda (bound, (params, e), body)

    | EXLambda bind ->
      let vars, params = lower_pattern vars bind.vb_patt in
      let e = lower_expr cont_atom vars bind.vb_expr in
      let r = fresh () in
      Expr_lambda (r, (params, e), cont (local r))

    | EXSequence (e1, e2) ->
      let e =
        lower_expr cont_atom vars e1,
        lower_expr cont_atom vars e2
      in
      let r = fresh () in
      Expr_sequence (r, e, cont (local r))

    | EXApplication (f, es) ->
      let f' = lower_expr cont_atom vars f in
      let es' = List.map (lower_expr cont_atom vars) es in
      let args = List.map get_atom es' in
      let r = fresh () in
      Expr_apply (r, (get_atom f', args), cont (local r))

    | EXTuple _ -> failwith "lower EXTuple"
    | EXConstruct _ -> failwith "lower EXConstruct"

    | EXLiteral l -> cont (lower_literal l)
    | EXIdentifier i -> cont (Atom_var (IMap.find i.item vars))      

    | EXMagical _ -> failwith "lower EXMagical"

    | EXError -> failwith "cannot lower invalid expression"

let lower_toplevel vars (tl : toplevel node) =
  match tl.item with
    | TLValueDef vdef ->
      let var = mangle_ident vdef.vd_name.item in
      let expr = lower_expr cont_atom vars vdef.vd_expr in
      let stmt = Stmt_definition (var, expr) in
      let vars = bind_global vars vdef.vd_name.item var in
      vars, Some stmt

    | TLExternDef edef ->
      let var = mangle_ident edef.ed_name.item in
      let stmt = Stmt_external (var, edef.ed_native_name.item) in
      let vars = bind_global vars edef.ed_name.item var in
      vars, Some stmt
    
    | _ -> vars, None

let lower_tast tast =
  let rec go vars program = function
    | [] -> program
    | tl :: tast ->
      let vars, stmt = lower_toplevel vars tl in
      go vars (program @ [stmt]) tast
  in
  go IMap.empty [] tast
  |> List.filter_map (fun x -> x)
