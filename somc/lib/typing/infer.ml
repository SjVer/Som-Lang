open Types
open Tast
open Unify

module Ident = Symbols.Ident
module Ast = Parse.Ast

(* helper functions *)

let mk span typ item = {span; item; typ}

let set_ty typ n = {n with typ}

(* inference functions *)

let infer_patt level env (patt : Ast.pattern node) =
  let env', item', typ = match patt.item with
    | PAVariable v ->
      let v_typ = new_var level in
      let env' = Env.add_value env (Ident v) v_typ in
      env', PAVariable v, v_typ
    | PAWildcard ->
      env, PAWildcard, new_var level
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
    | EXGrouping e ->
      let t = infer_expr level env e in
      mk s t.typ (EXGrouping t) 
    
    | EXBinding (bind, body) ->
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
      mk s body'.typ (EXBinding (bind', body'))
    
    | EXLambda bind ->
      let env', patt' = infer_patt level env bind.vb_patt in
      let expr' = infer_expr level env' bind.vb_expr in
      (* construct the updated binding *)
      let binding = {vb_patt = patt'; vb_expr = expr'} in
      mk s (TFun (patt'.typ, expr'.typ)) (EXLambda binding)
    
    | EXSequence (e1, e2) ->
      let e1' = infer_expr level env e1 in
      let e2' = infer_expr level env e2 in
      mk s e2'.typ (EXSequence (e1', e2'))

    | EXConstraint (e, t) ->
      let t' = Parse_type.parse env level t.item in
      let e' = infer_expr level env e in
      begin
        try unify ~do_raise:true env s t' e'.typ
        with Report.Error r ->
          Report.add_note "type expected by type constraint." r
          |> Report.report
      end;
      set_ty t' e'

    | EXApplication (f, es) ->
      assert (es <> []);
      let f' = infer_expr level env f in
      let out_ty, es' = infer_and_unify_appl level env f'.span f'.typ es in
      mk s out_ty (EXApplication (f', es'))

    | EXTuple es ->
      let es' = List.map (infer_expr level env) es in
      let ts = List.map (fun e -> e.typ) es' in
      mk s (TTup ts) (EXTuple es')

    | EXConstruct (i, es) ->
      let constr_typ = instantiate level (Env.lookup_value env i.item) in
      let out_ty, es' = infer_and_unify_appl level env i.span constr_typ es in
      mk s out_ty (EXConstruct (mk i.span constr_typ i.item, es'))

    | EXLiteral l ->
      (* TODO: look these up instead? *)
      let name n = TName (Cons ("_std_types", Ident n)) in
      let l', t = match l with
        | LIChar c   -> LIChar c,   TPrim (PInt (false, 8))
        | LIFloat f  -> LIFloat f,  TVague (ref VGFloat)
        | LIInt i    -> LIInt i,    TVague (ref VGInt) 
        | LINil      -> LINil,      TPrim PVoid
        | LIString s -> LIString s, name "Str"
      in mk s t (EXLiteral l')
    
    | EXIdentifier {span; item} ->
      let t = instantiate level (Env.lookup_value env item) in
      mk s t (EXIdentifier (mk span t item)) 

    | EXMagical n ->
      begin try
        let m = Magicals.find n in
        let t = Magicals.type_of m in
        mk s t (EXMagical m)
      with Not_found ->
        error (Use_of_invalid_magical n) s [] |> Report.report; 
        mk s TError EXError
      end

    | EXError -> mk s TError EXError

(* helper functions *)

let infer_expr env e =
  let e' =
    try infer_expr 0 env e
    with Report.Error err ->
      Report.report err;
      let span = {e.span with Span.ghost=true} in
      mk span (new_var 0) EXError
  in
  (* e' *)
  set_ty (generalize (-1) e'.typ) e'