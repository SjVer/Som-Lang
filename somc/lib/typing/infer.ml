open Types
open Tast
open Unify

module Ident = Symbols.Ident
module Ast = Parse.Ast

(* helper functions *)

let mk span typ item = {span; item; typ}

let set_ty typ n = {n with typ}

(* inference functions *)

let infer_literal =
  let open Ast in function
    | Pli_int i    -> Tli_int i,    TPrim PInt
    | Pli_char c   -> Tli_char c,   TPrim PChar
    | Pli_float f  -> Tli_float f,  TPrim PFloat
    | Pli_string s -> Tli_string s, TPrim PString
    | Pli_null     -> Tli_null,     TPrim PNil

let rec fold_application_type env span fty = function
  | n :: ns ->
    let param_ty, out_ty = match_fun_ty span fty in
    unify env n.span param_ty n.typ;

    let new_span = Span.concat_spans span n.span in
    fold_application_type env new_span out_ty ns
  | [] -> fty

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
    
    | Ppat_construct (i, args) ->
      let constr_typ = instantiate level (Env.lookup_value env i.item) in
      let f (env, patts') patt =
        let env, patt' = infer_patt level env patt in
        env, patts' @ [patt']
      in
      let env, args' = List.fold_left f (env, []) args in

      let t = fold_application_type env patt.span constr_typ args' in
      env, Tpat_construct (mk i.span constr_typ i.item, args'), t
    
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

let rec infer_cases level env cases =
  let scrut_t = new_var level in
  let action_t = new_var level in
  let infer (patt, expr) =
    let env, patt' = infer_patt level env patt in
    unify env patt'.span scrut_t patt'.typ;

    let expr' = infer_expr level env expr in
    unify env expr'.span action_t expr'.typ;

    patt', expr'
  in
  scrut_t, action_t, List.map infer cases

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
    
    | Pexp_match (e, cases) ->
      let e' = infer_expr level env e in
      let scrut_t, action_t, cases' = infer_cases (level + 1) env cases in
      unify env e'.span scrut_t e'.typ;
      mk s action_t (Texp_match (e', cases'))
    
    | Pexp_switch cases ->
      let scrut_t, action_t, cases' = infer_cases (level + 1) env cases in
      let t = TFun (scrut_t, action_t) in
      mk s t (Texp_switch cases')

    | Pexp_if (cond, texp, eexp) ->
      let cond' = infer_expr level env cond in
      (* TODO: unify with boolean type? *)

      let texp' = infer_expr level env texp in
      let eexp' = infer_expr level env eexp in

      begin
        try unify ~do_raise:true env eexp.span texp'.typ eexp'.typ
        with Report.Error r ->
          let msg = ("if-branch has type " ^ show texp'.typ false) in
          Report.add_related (`Note msg) texp.span r
          |> Report.report
      end;

      mk s texp'.typ (Texp_if (cond', texp', eexp'))

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
      let es' = List.map (infer_expr level env) es in
      let out_ty = fold_application_type env s f'.typ es' in
      mk s out_ty (Texp_apply (f', es'))

    | Pexp_tuple es ->
      let es' = List.map (infer_expr level env) es in
      let ts = List.map (fun e -> e.typ) es' in
      mk s (TTup ts) (Texp_tuple es')

    | Pexp_construct (i, es) ->
      let constr_typ = instantiate level (Env.lookup_value env i.item) in
      let es' = List.map (infer_expr level env) es in
      let out_ty = fold_application_type env s constr_typ es' in
      mk s out_ty (Texp_construct (mk i.span constr_typ i.item, es'))

    | Pexp_literal l ->
      let l', t = infer_literal l in
      mk s t (Texp_literal l')
    
    | Pexp_ident {span; item} ->
      let t = instantiate level (Env.lookup_value env item) in
      mk s t (Texp_ident (mk span t item)) 

    | Pexp_primitive n ->
      begin try
        let p = Symbols.Primitive.find n in
        mk s (new_var level) (Texp_primitive p)
      with Not_found ->
        error (Use_of_invalid_primitive n) s [] |> Report.report; 
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
