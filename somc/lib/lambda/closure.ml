open Ir

module SSet = Set.Make(String)

let free_vars =
  let open SSet in
  let (@) = union in
  let in_atom = function
    | Atom_var (Var_local v) -> singleton v
    | _ -> empty
  in
  let rec go = function
    | Expr_let (name, value, expr) ->
      let in_value = diff (go value) (singleton name) in
      in_value @ remove name (go expr)
    | Expr_lambda (params, expr) ->
      diff (go expr) (of_list params)

    | Expr_apply (f, args) ->
      let vs = List.map in_atom args in
      in_atom f @ of_list (List.map SSet.choose vs)
    | Expr_if (cond, thenexpr, elseexpr) ->
      in_atom cond @ go thenexpr @ go elseexpr
    | Expr_sequence (e1, e2) -> go e1 @ go e2
    | Expr_tuple els ->
      let vs = List.map in_atom els in
      of_list (List.map SSet.choose vs)
    | Expr_lazy e -> go e
    | Expr_get (Var_local v, _)
    | Expr_eval (Var_local v) -> singleton v
    | Expr_atom a -> in_atom a
    | _ -> empty
  in
  go

(* conversion stuff *)

let rec convert_expr = function
  | Expr_let (name, value, expr) ->
    Expr_let (name, convert_expr value, convert_expr expr)

  | Expr_lambda (params, body) ->
    let env = Lower.mangle "env" in
    let free_vars = SSet.(elements
      (diff (free_vars body) (of_list params)))
    in

    if free_vars <> [] then
      (* helper function to create "let x = env[i] in e" *)
      let get (e, i) x = (
        Expr_let (x, Expr_get (Var_local env, i), e),
        i + 1)
      in
      (* greates the `get`s from `env` *)
      let body = fst (List.fold_left
        get (convert_expr body, 1) free_vars)
      in
      let lam = Expr_lambda (env :: params, body) in
      let f = Lower.mangle "f" in
      let thunk =
        let els = List.map Lower.local free_vars in
        Expr_apply (Lower.local f, els)
      in
      (* let f = \env vars -> ... in (f, vars) *)
      Expr_let (f, lam, thunk)
    else
      Expr_lambda (params, convert_expr body)

  | Expr_if (cond, thenexpr, elseepxr) ->
    Expr_if (cond, convert_expr thenexpr, convert_expr elseepxr)
  | Expr_sequence (e1, e2) ->
    Expr_sequence (convert_expr e1, convert_expr e2)
  | Expr_lazy e -> convert_expr e
  | expr -> expr

let convert_stmt = function
  | Stmt_definition (name, expr) ->
    (* TODO: params treated as free vars *)
    Stmt_definition (name, convert_expr expr)
  | stmt -> stmt

let convert_program = List.map convert_stmt