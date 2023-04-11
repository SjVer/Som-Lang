open Ir

(* lambda lifting stuff *)

let rec lift_expr = function
  | Expr_let (name, value, expr) ->
    let vfuncs, value = lift_expr value in
    let efuncs, expr = lift_expr expr in
    vfuncs @ efuncs, Expr_let (name, value, expr)

  | Expr_lambda (params, body) ->
    let f = Lower.mangle "lam" in
    let funcs, body = lift_expr body in
    let expr = Expr_atom (Atom_var (Var_global f)) in
    let lam = Expr_lambda (params, body) in
    funcs @ [Stmt_definition (f, lam)], expr

  | Expr_if (cond, texpr, eexpr) ->
    let tfuncs, texpr = lift_expr texpr in
    let efuncs, eexpr = lift_expr eexpr in
    tfuncs @ efuncs, Expr_if (cond, texpr, eexpr)

  | Expr_sequence (e1, e2) ->
    let funcs1, e1 = lift_expr e1 in
    let funcs2, e2 = lift_expr e2 in
    funcs1 @ funcs2, Expr_sequence (e1, e2)

  | Expr_lazy e ->
    let funcs, e = lift_expr e in
    funcs, Expr_lazy e

  | expr -> [], expr

let lift_stmt = function
  | Stmt_definition (name, Expr_lambda (params, body)) ->
    (* prevent `(define lam! ...) (define name! lam!)` *)
    let funcs, body = lift_expr body in
    funcs @ [Stmt_definition (name, Expr_lambda (params, body))]
  | Stmt_definition (name, value) ->
    let funcs, value = lift_expr value in
    funcs @ [Stmt_definition (name, value)]
  (* | Stmt_definition (name, value) ->
    let funcs, value = lift_expr value in
    funcs @ [Stmt_definition (name, value)] *)
  | stmt -> [stmt]

let lift_program program =
  List.map lift_stmt program
  |> List.flatten