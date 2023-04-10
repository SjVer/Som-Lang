open Ir

let rec uncurry_expr = function
  | Expr_lambda (params, (Expr_lambda _ as lam)) ->
    let [@ warning "-8"] Expr_lambda (params', body) =
      uncurry_expr lam
    in
    Expr_lambda (params @ params', body)
  | expr -> expr

let uncurry_stmt = function
  | Stmt_definition (name, value) ->
    Stmt_definition (name, uncurry_expr value)
  | stmt -> stmt

let uncurry_program = List.map uncurry_stmt
