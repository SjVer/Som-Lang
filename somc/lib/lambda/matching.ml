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

(* compiling matches *)

let rec compile_patt_check patt =
  match patt.item with
    | Tpat_wildcard
    | Tpat_variable _ -> Check_done
    | Tpat_literal l ->
      Check_const (lower_literal l)
    | Tpat_tuple patts ->
      let checks = List.map compile_patt_check patts in
      Check_tuple checks

let compile_patt_extractors env patt =
  let rec go acc patt = 
    match patt.item with
      | Tpat_wildcard -> []
      | Tpat_variable v ->
        [find_local env v, acc]
      | Tpat_literal _ -> [] 
      | Tpat_tuple patts ->
        let f (acc', i) patt =
          acc' @ go (acc @ [Extr_get i]) patt,
          i + 1
        in
        List.fold_left f ([], 0) patts |> fst
  in
  go [] patt

let compile_patt env patt action =
  let check = compile_patt_check patt in
  let extractors = compile_patt_extractors env patt in
  {check; extractors; action}

(* lowering *)

let rec bind_patt_vars env patt =
  match patt.item with
    | Tpat_wildcard -> env
    | Tpat_variable v ->
      let var = Env.mangle v in
      Env.bind_local env (Ident v) var
    | Tpat_literal _ -> env
    | Tpat_tuple patts ->
      List.fold_left bind_patt_vars env patts

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
      let case = compile_patt env patt body in
      let r = Env.fresh () in
      let r' = Atom_var (Var_local r) in
      Expr_lambda ([r], Expr_match (r', [case]))
      (* failwith "lower_lambda" *)