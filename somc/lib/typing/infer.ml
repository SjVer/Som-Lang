open Types
open Tast
open Unify

module Ident = Symbols.Ident
module Ast = Parse.Ast

(* helper functions *)

let mk span typ item = {span; item; typ}

let set_ty typ n = {n with typ}

(* inference functions *)

let infer_magic =
  let open Symbols.Magic in
  (* TODO: expand on this *)
  let int = TName (Ident "Int") in
  let int_fn = TFun (int, int) in
  let ints_fn = TFun (int, int_fn) in
  function
    | Magic_add
    | Magic_sub 
    | Magic_mul 
    | Magic_div  -> ints_fn 
    | Magic_rem  -> ints_fn
    | Magic_abs
    | Magic_neg  -> int_fn 
    | Magic_and
    | Magic_or   -> ints_fn
    | Magic_not  -> int_fn
    | Magic_eq
    | Magic_gt 
    | Magic_lt 
    | Magic_neq 
    | Magic_gteq
    | Magic_lteq -> ints_fn 

let infer_literal =
  let name n = TName (Ident n) in
  let open Ast in function
    | Pli_int i    -> Tli_int i,    name "Int"
    | Pli_char c   -> Tli_char c,   name "Chr"
    | Pli_float f  -> Tli_float f,  name "Flt"
    | Pli_null     -> Tli_null,     name "Nil"
    | Pli_string s -> Tli_string s, name "Str"

let rec infer_patt level env (patt : Ast.pattern node) =
  let env', item', typ = match patt.item with
    | Ppat_wildcard -> env, Tpat_wildcard, new_var level
    | Ppat_variable v ->
      let v_typ = new_var level in
      let env = Env.add_value env (Ident v) v_typ in
      env, Tpat_variable v, v_typ
    | Ppat_literal l ->
      let l', t = infer_literal l in
      env, Tpat_literal l', t
    | Ppat_tuple patts ->
      let f (env, patts') patt =
        let env, patt' = infer_patt level env patt in
        env, patts' @ [patt']
      in
      let env, patts' = List.fold_left f (env, []) patts in
      let ts = List.map (fun n -> n.typ) patts' in
      env, Tpat_tuple patts', TTup ts
  in
  env', mk patt.span typ item'

(* infer and unify the application of arguments. e.g.:
    infer (A -> B -> C) [a, b]
      calls infer (B -> C) [b]
        returns C, [b']
      returns C, [a', b'] *)
let rec infer_and_unify_appl level env span fty = function
  | (e : Ast.expr node) :: es ->
    let param_ty, out_ty = match_fun_ty span fty in
    let e' = infer_expr level env e in
    unify env e'.span param_ty e'.typ;

    let new_span = Span.concat_spans span e.span in
    let next_out_ty, es' = infer_and_unify_appl
      level env new_span out_ty es
    in
    next_out_ty, e' :: es'
  | [] -> fty, []

(** infer an expression *)
and infer_expr level env exp =
  let {span = s; item = exp} : Ast.expr Ast.node = exp in
  match exp with
    | Pexp_grouping e ->
      let t = infer_expr level env e in
      mk s t.typ (Texp_grouping t) 
    
    | Pexp_binding (bind, body) ->
      (* the bound expr stands apart from the pattern its bound to *)
      let expr' = infer_expr (level + 1) env bind.vb_expr in
      (* the bound expr and its pattern should have the same type *)
      let env', patt' = infer_patt level env bind.vb_patt in
      unify env s patt'.typ expr'.typ;
      (* infer the body *)
      let body' = infer_expr level env' body in
      let expr'' = set_ty (generalize level expr'.typ) expr' in
      (* construct the updated binding *)
      let bind' = {vb_patt = patt'; vb_expr = expr''} in
      mk s body'.typ (Texp_binding (bind', body'))
    
    | Pexp_lambda bind ->
      let env', patt' = infer_patt level env bind.vb_patt in
      let expr' = infer_expr level env' bind.vb_expr in
      (* construct the updated binding *)
      let binding = {vb_patt = patt'; vb_expr = expr'} in
      mk s (TFun (patt'.typ, expr'.typ)) (Texp_lambda binding)
    
    | Pexp_sequence (e1, e2) ->
      let e1' = infer_expr level env e1 in
      let e2' = infer_expr level env e2 in
      mk s e2'.typ (Texp_sequence (e1', e2'))

    | Pexp_constraint (e, t) ->
      let t' = Parse_type.parse env level t.item in
      let e' = infer_expr level env e in
      begin
        try unify ~do_raise:true env s t' e'.typ
        with Report.Error r ->
          Report.add_note "type is enforced by type constraint." r
          |> Report.report
      end;
      set_ty t' e'

    | Pexp_apply (f, es) ->
      assert (es <> []);
      let f' = infer_expr level env f in
      let out_ty, es' = infer_and_unify_appl level env f'.span f'.typ es in
      mk s out_ty (Texp_apply (f', es'))

    | Pexp_tuple es ->
      let es' = List.map (infer_expr level env) es in
      let ts = List.map (fun e -> e.typ) es' in
      mk s (TTup ts) (Texp_tuple es')

    | Pexp_construct (i, es) ->
      let constr_typ = instantiate level (Env.lookup_value env i.item) in
      let out_ty, es' = infer_and_unify_appl level env i.span constr_typ es in
      mk s out_ty (Texp_construct (mk i.span constr_typ i.item, es'))

    | Pexp_literal l ->
      let l', t = infer_literal l in
      mk s t (Texp_literal l')
    
    | Pexp_ident {span; item} ->
      let t = instantiate level (Env.lookup_value env item) in
      mk s t (Texp_ident (mk span t item)) 

    | Pexp_magic n ->
      begin try
        let m = Symbols.Magic.find n in
        let t = infer_magic m in
        mk s t (Texp_magic m)
      with Not_found ->
        error (Use_of_invalid_magical n) s [] |> Report.report; 
        mk s TError Texp_error
      end

    | Pexp_error -> mk s TError Texp_error

(* helper functions *)

let infer_expr env e =
  let e' =
    try infer_expr 0 env e
    with Report.Error err ->
      Report.report err;
      let span = {e.span with Span.ghost=true} in
      mk span (new_var 0) Texp_error
  in
  (* e' *)
  set_ty (generalize (-1) e'.typ) e'