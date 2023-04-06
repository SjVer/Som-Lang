open Typing.TAst
open Ir

(* variables stuff *)

module IMap = Map.Make(Ident)

let add_var kind ident =
  let ident' = Ident.to_string ident in
  match kind with
    | `Global -> IMap.add ident (Var_global ident')
    | `Local -> IMap.add ident (Var_local ident')

let fresh =
  let c = ref 0 in
  fun () ->
    incr c;
    "_" ^ string_of_int !c

let local r = Atom_var (Var_local r)

let cont_atom a = Expr_atom a
let cont_eval = function
  | Atom_const _ as a -> Expr_atom a
  | Atom_var v ->
    let r = fresh () in
    Expr_eval (r, v, cont_atom (local r))

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

let rec lower_expr cont vars expr =
  match expr.item with
    | EXGrouping e -> lower_expr cont vars e

    | EXBinding _ -> failwith "lower EXBinding"
    | EXLambda _ -> failwith "lower EXLambda"

    | EXSequence (e1, e2) ->
      let e =
        lower_expr cont_atom vars e1,
        lower_expr cont_atom vars e2
      in
      let r = fresh () in
      Expr_sequence (r, e, cont (local r))

    | EXApplication _ -> failwith "lower EXApplication"
    | EXTuple _ -> failwith "lower EXTuple"
    | EXConstruct _ -> failwith "lower EXConstruct"

    | EXLiteral l -> cont (lower_literal l)
    | EXIdentifier i -> cont (Atom_var (IMap.find i.item vars))      

    | EXMagical _ -> failwith "lower EXMagical"

    | EXError -> failwith "cannot lower invalid expression"

let lower_toplevel vars (tl : toplevel node) =
  match tl.item with
    | TLValueDef vdef ->
      let stmt = Stmt_definition (
        Ident.to_string vdef.vd_name.item,
        lower_expr cont_atom vars vdef.vd_expr)
      in
      let vars = add_var `Global vdef.vd_name.item vars in
      vars, Some stmt

    | TLExternDef edef ->
      let stmt = Stmt_external (
        Ident.to_string edef.ed_name.item,
        edef.ed_native_name.item)
      in
      let vars = add_var `Global edef.ed_name.item vars in
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
