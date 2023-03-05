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
    | PA_Variable v ->
      let v_typ = new_var level in
      let env' = Env.add_value env (Ident v) v_typ in
      env', PA_Variable v, v_typ
    | PA_Wildcard ->
      env, PA_Wildcard, new_var level
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
    | EX_Grouping e ->
      let t = infer_expr level env e in
      mk s t.typ (EX_Grouping t) 
    
    | EX_Binding (bind, body) ->
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
      mk s body'.typ (EX_Binding (bind', body'))
    
    | EX_Lambda bind ->
      let env', patt' = infer_patt level env bind.vb_patt in
      let expr' = infer_expr level env' bind.vb_expr in
      (* construct the updated binding *)
      let binding = {vb_patt = patt'; vb_expr = expr'} in
      mk s (TFun (patt'.typ, expr'.typ)) (EX_Lambda binding)
    
    | EX_Sequence (e1, e2) ->
      let e1' = infer_expr level env e1 in
      let e2' = infer_expr level env e2 in
      mk s e2'.typ (EX_Sequence (e1', e2'))

    | EX_Constraint (e, t) ->
      let t' = Parse_type.parse env level t.item in
      let e' = infer_expr level env e in
      begin
        try unify ~do_raise:true env s t' e'.typ
        with Report.Error r ->
          Report.add_note "type expected by type constraint." r
          |> Report.report
      end;
      set_ty t' e'

    | EX_Application (f, es) ->
      assert (es <> []);
      let f' = infer_expr level env f in
      let out_ty, es' = infer_and_unify_appl level env f'.span f'.typ es in
      mk s out_ty (EX_Application (f', es'))

    | EX_Tuple es ->
      let es' = List.map (infer_expr level env) es in
      let ts = List.map (fun e -> e.typ) es' in
      mk s (TTup ts) (EX_Tuple es')

    | EX_Construct (i, es) ->
      let constr_typ = instantiate level (Env.lookup_value env i.item) in
      let out_ty, es' = infer_and_unify_appl level env i.span constr_typ es in
      mk s out_ty (EX_Construct (mk i.span constr_typ i.item, es'))

    | EX_Literal l ->
      (* TODO: look these up instead? *)
      let name n = TName (Cons ("_std_types", Ident n)) in
      let l', t = match l with
        | LI_Char c   -> LI_Char c,   name "Chr"
        | LI_Float f  -> LI_Float f,  TVague (ref Float)
        | LI_Int i    -> LI_Int i,    TVague (ref Int) 
        | LI_Nil      -> LI_Nil,      name "Nll"
        | LI_String s -> LI_String s, name "Str"
      in mk s t (EX_Literal l')
    
    | EX_Identifier {span; item} ->
      let t = instantiate level (Env.lookup_value env item) in
      mk s t (EX_Identifier (mk span t item)) 

    | EX_External n ->
      let t = instantiate level (Hashtbl.find Env.externals n) in
      mk s t (EX_External n)
    
    | EX_Magical _ ->
      (* TODO: implement this *)
      mk s TError EX_Error

    | EX_Error -> mk s TError EX_Error

(* helper functions *)

let infer_expr env e =
  let e' =
    try infer_expr 0 env e
    with Report.Error err ->
      Report.report err;
      let span = {e.span with Span.ghost=true} in
      mk span (new_var 0) EX_Error
  in
  (* e' *)
  set_ty (generalize (-1) e'.typ) e'