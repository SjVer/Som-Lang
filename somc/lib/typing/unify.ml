open Types
open Report.Error

let error do_raise e span =
  let r = Report.make_error (Type_error e) span in
  if do_raise then Report.raise r
  else Report.report r

(** generalizes type [ty] replacing unbound
    type variables with generic ones, and
    vague types with generic vague ones *)
let rec generalize level = function
  | TVar ({contents = Unbound (id, other_level)} as ty)
    when other_level > level ->
      ty := Generic id;
      TVar ty
  | TVague ({contents = Int | Float} as k) ->
    k := Vague !k;
    TVague k

  | TVar {contents = Solved ty} -> generalize level ty

  | TEff t -> TEff (generalize level t)
  | TFun (p, r) -> TFun (generalize level p, generalize level r)
  | TApp (a, t) -> TApp (generalize level a, generalize level t)
  | TTup ts -> TTup (List.map (generalize level) ts)
  
  | TVar {contents = Generic _}
  | TVar {contents = Unbound _}
  | TName _ | TPrim _ | TVague _
  | TError | TNever as ty -> ty

(** instantiates type [ty] replacing generic
    type variables with fresh ones and generic
    vague types with normal vague types *)
let instantiate level ty =
  let id_var_map = Hashtbl.create 20 in
  let rec go ty = match ty with
    | TVar {contents = Solved ty} -> go ty
    | TVar {contents = Generic id} -> begin
        try Hashtbl.find id_var_map id
        with Not_found ->
          let var = new_var level in
          Hashtbl.add id_var_map id var;
          var
      end
    | TVar {contents = Unbound _} -> ty
    | TVague {contents = Vague k} -> TVague (ref k)
    | TEff t -> TEff (go t)
    | TApp (a, t) -> TApp (go a, go t)
    | TFun (p, r) -> TFun (go p, go r)
    | TTup ts -> TTup (List.map go ts)
    | TName _ | TPrim _ | TVague _ | TError | TNever -> ty
  in go ty

(** asserts that the type isn't recursive
  and solves constraints like {'a = 'b}
  if 'b comes from a higher level than 'a *)
let occurs_check_adjust_levels ?(do_raise=false) span id level =
  let rec go = function
    | TName _ | TPrim _ | TVague _ | TError | TNever -> ()
    | TVar {contents = Solved ty} -> go ty
    | TVar {contents = Generic _} -> ()
    | TVar ({contents = Unbound (other_id, other_level)} as other) ->
      if other_id = id then
        error do_raise Recursive_type (Some span);
      if other_level > level
        then other := Unbound (other_id, other_level)
        else ()
    | TEff t -> go t
    | TApp (a, t) -> go a; go t
    | TFun (p, r) -> go p; go r
    | TTup ts -> List.iter go ts
  in go

let rec can_unify_vague_ty env kind = function
  (* we unify vague types with primitives or other vague types *)
  | TName n -> can_unify_vague_ty env kind (Env.lookup_alias env n)
  | TPrim (PInt _) when kind = Int -> true
  | TPrim (PFloat _) when kind = Float -> true
  | TVague {contents = (Int | Float) as k} -> k = kind
  | TVague {contents = Link t}
  | TVar {contents = Solved t} -> can_unify_vague_ty env kind t
  | _ -> false

let rec unify_name ?(do_raise=false) env span ty1 ty2 =
  let add_alias_note n t =
    Printf.sprintf
      "type `%s` is an alias for `%s`."
      (Symbols.Ident.to_string n)
      (Types.show t false)
    |> Report.add_note
  in
  let error =
    if do_raise then Report.raise
    else Report.report
  in 
  match (ty1, ty2) with
    | TName n1, TName n2 ->
      if n1 = n2 then ()
      else
        let t1 = Env.lookup_alias env n1 in
        let t2 = Env.lookup_alias env n2 in
        begin
          try unify ~do_raise:true env span t1 t2
          with Report.Error r -> r
            |> add_alias_note n1 t1
            |> add_alias_note n2 t2
            |> error
        end

    | TName n, ty | ty, TName n ->
      let nty = Env.lookup_alias env n in
      begin
        try unify ~do_raise:true env span nty ty
        with Report.Error r -> r
          |> add_alias_note n nty
          |> error
      end

    | _ -> invalid_arg "unify_name"

and unify ?(do_raise=false) env span ty1 ty2 =
  if ty1 == ty2 then ()
  else match [@warning "-57"] (ty1, ty2) with
    | TName _, _ | _, TName _ ->
      unify_name ~do_raise env span ty1 ty2

    | TPrim p1, TPrim p2 when p1 = p2 -> ()

    | TVague ({contents = Int | Float} as k), ty
    | ty, TVague ({contents = Int | Float} as k)
      when can_unify_vague_ty env !k ty -> k := Link ty

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
      occurs_check_adjust_levels ~do_raise span id level ty;
      tvar := Solved ty

    | TTup ts1, TTup ts2 -> List.iter2 (unify env span) ts1 ts2

    | _ ->
      let ty1' = show (generalize (-1) ty1) false in
      let ty2' = show (generalize (-1) ty2) false in
      error do_raise (Expected (ty1', ty2')) (Some span)

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
    error false (Expected_function (show t false)) (Some span);
    TError, TError

