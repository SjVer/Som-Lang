open Types
open Tast
open Report.Error

module Ident = Symboltable.Ident
module Ast = Parse.Ast

let error ?(fatal=false) e span =
  let r = Report.make_error (Type_error e) span in
  Report.report r;
  if fatal then Report.exit 1
  
let mk s t i = {span=s; item=i; typ=t}

let set_ty t n = {n with typ=t}

(* helper functions *)

(** generalizes type [ty] replacing 
    unbound type variables with Generic ones *)
let rec generalize level = function
  | TVar ({contents = Unbound (id, other_level)} as ty)
    when other_level > level ->
      ty := Generic id;
      TVar ty

  | TVar {contents = Solved ty} -> generalize level ty

  | TEff t -> TEff (generalize level t)
  | TFun (p, r) -> TFun (generalize level p, generalize level r)
  | TApp (a, t) -> TApp (generalize level a, generalize level t)
  | TTup ts -> TTup (List.map (generalize level) ts)
  
  | TVar {contents = Generic _}
  | TVar {contents = Unbound _}
  | TName _ | TPrim _ | TVague _
  | TError | TNever as ty -> ty

(** instantiates type [ty] replacing Generic
    type variables with fresh ones *)
let instantiate level ty =
  let id_var_map = Hashtbl.create 20 in
  let rec go ty = match ty with
    | TName _ | TPrim _ | TVague _ | TError | TNever -> ty
    | TVar {contents = Solved ty} -> go ty
    | TVar {contents = Generic id} -> begin
        try Hashtbl.find id_var_map id
        with Not_found ->
          let var = new_var level in
          Hashtbl.add id_var_map id var;
          var
      end
    | TVar {contents = Unbound _} -> ty
    | TEff t -> TEff (go t)
    | TApp (a, t) -> TApp (go a, go t)
    | TFun (p, r) -> TFun (go p, go r)
    | TTup ts -> TTup (List.map go ts)
  in go ty

(** asserts that the type isn't recursive
  and solves constraints like {'a = 'b}
  if 'b comes from a higher level than 'a *)
let occurs_check_adjust_levels span id level =
  let rec go = function
    | TName _ | TPrim _ | TVague _ | TError | TNever -> ()
    | TVar {contents = Solved ty} -> go ty
    | TVar {contents = Generic _} -> ()
    | TVar ({contents = Unbound (other_id, other_level)} as other) ->
      if other_id = id then
        error ~fatal:true Recursive_type (Some span);
      if other_level > level
        then other := Unbound (other_id, other_level)
        else ()
    | TEff t -> go t
    | TApp (a, t) -> go a; go t
    | TFun (p, r) -> go p; go r
    | TTup ts -> List.iter go ts
  in go

let rec unify env span ty1 ty2 =
  let unify_names n1 n2 =
    n1 = n2 || begin
      try
        let t1 = Env.get_alias env n1 span in
        let t2 = Env.get_alias env n2 span in
        unify env span t1 t2;
        true
      with Not_found -> false
    end
  in
  let rec is_vague_ty kind = function
    | TName n -> is_vague_ty kind (Env.get_alias env n span)
    | TPrim (PInt _) when kind = Int -> true
    | TPrim (PFloat _) when kind = Float -> true
    | TVague {contents = (Int | Float) as k} -> k = kind
    | TVague {contents = Link t}
    | TVar {contents = Solved t} -> is_vague_ty kind t
    | _ -> false
  in

  if ty1 == ty2 then ()
  else match [@warning "-57"] (ty1, ty2) with
    | TName n1, TName n2 when unify_names n1 n2 -> ()

    (* works, but any unbound typevars will get linked
       to the contents of the name, not the name itself *)
    (* | TName n, ty | ty, TName n ->
      let ty2 = Env.get_alias env n span in
      unify env span ty ty2 *)

    | TPrim p1, TPrim p2 when p1 = p2 -> ()

    | TVague ({contents = Int | Float} as k), ty
    | ty, TVague ({contents = Int | Float} as k)
      when is_vague_ty !k ty -> k := Link ty

    | TVague {contents = Link ty1}, ty2
    | ty1, TVague {contents = Link ty2} ->
      unify env span ty1 ty2

    | TError, _ | _, TError
    | TNever, _ | _, TNever -> ()

    | TFun (p1, r1), TFun (p2, r2) ->
      unify env span p1 p2;
      unify env span r1 r2

    | TApp (a1, t1), TApp (a2, t2) ->
      unify env span a1 a2;
      unify env span t1 t2

    | TVar {contents = Solved ty1}, ty2
    | ty1, TVar {contents = Solved ty2} ->
      unify env span ty1 ty2

    | TVar ({contents = Unbound (id, level)} as tvar), ty
    | ty, TVar ({contents = Unbound (id, level)} as tvar) ->
      occurs_check_adjust_levels span id level ty;
      tvar := Solved ty

    | TTup ts1, TTup ts2 -> List.iter2 (unify env span) ts1 ts2

    | _ ->
      let ty1' = show (generalize (-1) ty1) false in
      let ty2' = show (generalize (-1) ty2) false in
      error (Expected (ty1', ty2')) (Some span)

(** asserts that the given type is a function type *)
let rec match_fun_ty span = function
  | TFun (p, r) -> p, r
  | TVar {contents = Solved ty} -> match_fun_ty span ty
  | TVar ({contents = Unbound (_, level)} as tvar) ->
    let param_ty = new_var level in
    let return_ty = new_var level in
    tvar := Solved (TFun (param_ty, return_ty));
    param_ty, return_ty
  | TError -> TError, TError
  | t ->
    error (Expected_function (show t false)) (Some span);
    TError, TError

(* inference functions *)

let infer_patt ?(level=0) env patt =
  let {span=s; item=patt} : Ast.pattern Ast.node = patt in
  match patt with
    | PA_Variable v ->
      let v' = new_var level in
      Env.add_symbol env v v';
      mk s v' (PA_Variable v)
    | PA_Wildcard -> mk s (new_var level) PA_Wildcard

(** infer an expression *)
let rec infer_expr ?(level=0) env exp =
  let {span=s; item=exp} : Ast.expr Ast.node = exp in
  match exp with
    | EX_Grouping e ->
      let t = infer_expr ~level env e in
      mk s t.typ (EX_Grouping t) 
    
    | EX_Binding (bind, body) ->
      let env' = Env.copy env in
      let patt' = infer_patt ~level env' bind.vb_patt in
      let expr' = infer_expr ~level:(level + 1) env bind.vb_expr in
      unify env s patt'.typ expr'.typ;

      let expr'' = set_ty (generalize level expr'.typ) expr' in
      let body' = infer_expr ~level env' body in
      mk s body'.typ (EX_Binding ({patt=patt'; expr=expr''}, body'))
    
    | EX_Lambda {vb_patt; vb_expr} ->
      let env' = Env.copy env in
      let patt' = infer_patt ~level env' vb_patt in
      let expr' = infer_expr ~level env' vb_expr in
      mk s (TFun (patt'.typ, expr'.typ))
        (EX_Lambda {patt=patt'; expr=expr'})
    
    | EX_Sequence (e1, e2) ->
      let e1' = infer_expr ~level env e1 in
      let e2' = infer_expr ~level env e2 in
      mk s e2'.typ (EX_Sequence (e1', e2'))
  
    | EX_Constraint (e, t) ->
      let t' = Parse_type.parse env level t.item in
      let e' = infer_expr ~level env e in
      begin
        try unify env s t' e'.typ
        with Report.Error _ ->
          let t'' = show t' false in
          let e'' = show e'.typ false in
          Report.make_error
            (Type_error (Expected (t'', e'')))
            (Some s)
          |> Report.add_note "type expected due to type constraint"
          |> Report.report
      end;
      set_ty t' e'

    | EX_Application (f, es) ->
      assert (es <> []);
      let f' = infer_expr ~level env f in

      (* infer and unify recursively. e.g.:
          go (A -> B -> C) [a, b]
            calls go (B -> C) [b]
              returns C, [b']
            returns C, [a', b'] *)
      let rec go span fty = function
        | e :: es ->
          let param_ty, out_ty = match_fun_ty span fty in
          let e' = infer_expr ~level env e in
          unify env e'.span param_ty e'.typ;

          let new_span = Span.concat_spans span e.span in
          let next_out_ty, es' = go new_span out_ty es in
          next_out_ty, e' :: es'
        | [] -> fty, []
      in

      let out_ty, es' = go f'.span f'.typ es in
      mk s out_ty (EX_Application (f', es'))

    | EX_Tuple es ->
      let es' = List.map (infer_expr ~level env) es in
      let ts = List.map (fun e -> e.typ) es' in
      mk s (TTup ts) (EX_Tuple es')

    | EX_Construct (pnode, _enode) ->
      let name = Ident.to_string pnode.item in
      error ~fatal:true (Use_of_unbound ("constructor", name)) (Some pnode.span);
      failwith "unreachable"

    | EX_Literal l ->
      let name n = TName (Ident.Cons (Ident.Ident "_std_types", n)) in
      let l', t = match l with
        | LI_Char c   -> LI_Char c,   name "Chr"
        | LI_Float f  -> LI_Float f,  TVague (ref Float)
        | LI_Int i    -> LI_Int i,    TVague (ref Int) 
        | LI_Nil      -> LI_Nil,      name "Nll"
        | LI_String s -> LI_String s, name "Str"
      in mk s t (EX_Literal l')
    
    | EX_Identifier {span; item} ->
      begin try
        let t = instantiate level (Env.get_symbol env item span) in
        mk s t (EX_Identifier (mk span t item)) 
      with Not_found ->
        let name = Ident.to_string item in
        error (Use_of_unbound ("variable", name)) (Some span);
        mk s TError (EX_Identifier (mk span TError item))
      end

    | EX_External n ->
      let t = begin try
        instantiate level (Env.SMap.find n !Env.externals)
      with Not_found ->
        let t' = new_var level in
        Env.externals := Env.SMap.add n t' !Env.externals;
        t'
      end in
      mk s t (EX_External n)
    
    | EX_Magical _ -> failwith "infer_expr magical"

    | EX_Error -> mk s TError EX_Error

(* helper functions *)

let infer_expr env e =
  let e' =
    try infer_expr ~level:0 env e
    with Report.Error err ->
      Report.report err;
      let span = {e.span with Span.ghost=true} in
      mk span (new_var 0) EX_Error
  in
  (* e' *)
  set_ty (generalize (-1) e'.typ) e'