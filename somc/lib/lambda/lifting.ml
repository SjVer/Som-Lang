open Ir

(* lambda lifting stuff *)

let rec lift_expr = function
  | Expr_lambda (params, body) ->
    let f = Lower.mangle "lam" in
    let funcs, body = lift_expr body in
    let expr = Expr_atom (Lower.local f) in
    funcs @ [Stmt_function (f, params, body)], expr
  | expr -> [], expr

let lift_stmt = function
  | Stmt_definition (name, Expr_lambda (params, body)) ->
    let funcs, body = lift_expr body in
    funcs @ [Stmt_function (name, params, body)]
  | Stmt_definition (name, value) ->
    let funcs, value = lift_expr value in
    funcs @ [Stmt_definition (name, value)]
  | stmt -> [stmt]

let lift_program program =
  List.map lift_stmt program
  |> List.flatten