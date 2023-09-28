open Ir

module SSet = Set.Make(String)

let free_vars =
  let open SSet in
  let (@) = union in
  let in_atom = function
    | Latom_var (Lvar_local v) -> singleton v
    | _ -> empty
  in
  let flatten setlist =
    List.map elements setlist
    |> List.flatten |> of_list
  in
  let rec go = function
    | Lexpr_let (name, value, expr) ->
      let in_value = diff (go value) (singleton name) in
      in_value @ remove name (go expr)
    | Lexpr_lambda (params, expr) ->
      diff (go expr) (of_list params)
    | Lexpr_call (f, args) ->
      let vs = List.map go args in
      in_atom f @ flatten vs
    | Lexpr_apply (f, args) ->
      let vs = List.map go args in
      go f @ flatten vs
    | Lexpr_if (cond, thenexpr, elseexpr) ->
      go cond @ go thenexpr @ go elseexpr
    | Lexpr_sequence (e1, e2) -> go e1 @ go e2
    | Lexpr_tuple els ->
      let vs = List.map in_atom els in
      flatten vs
    | Lexpr_lazy e -> go e
    | Lexpr_get (Lvar_local v, _)
    | Lexpr_eval (Lvar_local v) -> singleton v
    | Lexpr_atom a -> in_atom a
    | _ -> empty
  in
  go

(* conversion stuff *)

let rec convert_expr = function
  | Lexpr_let (name, value, expr) ->
    Lexpr_let (name, convert_expr value, convert_expr expr)

  | Lexpr_lambda (params, body) ->
    let env = Env.mangle "env" in
    let free_vars = SSet.(elements
      (diff (free_vars body) (of_list params)))
    in

    if List.length free_vars > 0 then
      (* helper function to create "let x = env[i] in e" *)
      let get (e, i) x = (
        Lexpr_let (x, Lexpr_get (Lvar_local env, i), e),
        i + 1)
      in
      (* greates the `get`s from `env` *)
      let body = fst (List.fold_left
        get (convert_expr body, 1) free_vars)
      in
      let lam = Lexpr_lambda (env :: params, body) in
      let f = Env.mangle "f" in
      let thunk =
        let els = List.map (fun v -> Lexpr_atom (Lower.local v)) free_vars in
        Lexpr_apply (Lexpr_atom (Lower.local f), els)
      in
      (* let f = \env vars -> ... in (f, vars) *)
      Lexpr_let (f, lam, thunk)
    else
      Lexpr_lambda (params, convert_expr body)

  | Lexpr_if (cond, texpr, eexpr) ->
    Lexpr_if (convert_expr cond, convert_expr texpr, convert_expr eexpr)
  | Lexpr_sequence (e1, e2) ->
    Lexpr_sequence (convert_expr e1, convert_expr e2)
  | Lexpr_lazy e -> convert_expr e
  | expr -> expr

let convert_stmt = function
  | Lstmt_definition (name, expr) ->
    Lstmt_definition (name, convert_expr expr)
  | stmt -> stmt

let convert_program = List.map convert_stmt
