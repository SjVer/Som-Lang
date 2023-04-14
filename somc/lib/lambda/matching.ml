open Typing.TAst
open Ir

let find_local env v=
  match Env.find env (Ident v) with
    | Var_local var -> var
    | _ -> invalid_arg "lower_binding Tpat_variable"

let lower_literal = function
  | Tli_int i -> Const_int i
  | Tli_char c -> Const_int (Char.code c)
  | Tli_float f -> Const_float f
  | Tli_string s -> Const_string s
  | Tli_null -> Const_nil

(* generating trees *)

(* compiling trees *)

(* lowering *)

let lower_binding env patt value body =
  match patt.item with
    | Tpat_wildcard -> Expr_sequence (value, body)
    | Tpat_variable v ->
      let var = find_local env v in
      Expr_let (var, value, body)
    | _ -> failwith "lower_binding"
    (* | patt -> compile_match env [(patt, value)] *)

let lower_lambda env patt body =
  match patt.item with
    | Tpat_wildcard ->
      Expr_lambda ([Env.mangle "_"], body)
    | Tpat_variable v ->
      let var = find_local env v in
      Expr_lambda ([var], body)
    | _ ->
      (* let case = compile_patt env patt body in
      let r = Env.fresh () in
      let r' = Atom_var (Var_local r) in
      Expr_lambda ([r], Expr_match (r', [case])) *)
      failwith "lower_lambda"