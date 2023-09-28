open Ir

(* lambda lifting stuff *)

let rec lift_expr = function
  | Lexpr_let (name, value, expr) ->
    let vfuncs, value = lift_expr value in
    let efuncs, expr = lift_expr expr in
    vfuncs @ efuncs, Lexpr_let (name, value, expr)

  | Lexpr_lambda (params, body) ->
    let f = Env.mangle "lam" in
    let funcs, body = lift_expr body in
    let expr = Lexpr_atom (Latom_var (Lvar_global f)) in
    funcs @ [Lstmt_function (f, params, body)], expr

  | Lexpr_if (cond, texpr, eexpr) ->
    let tfuncs, texpr = lift_expr texpr in
    let efuncs, eexpr = lift_expr eexpr in
    tfuncs @ efuncs, Lexpr_if (cond, texpr, eexpr)

  | Lexpr_sequence (e1, e2) ->
    let funcs1, e1 = lift_expr e1 in
    let funcs2, e2 = lift_expr e2 in
    funcs1 @ funcs2, Lexpr_sequence (e1, e2)

  | Lexpr_lazy e ->
    let funcs, e = lift_expr e in
    funcs, Lexpr_lazy e

  | expr -> [], expr

let lift_stmt = function
  | Lstmt_definition (name, Lexpr_lambda (params, body)) ->
    let funcs, body = lift_expr body in
    funcs @ [Lstmt_function (name, params, body)]

  | Lstmt_definition (name, value) ->
    let funcs, value = lift_expr value in
    funcs @ [Lstmt_definition (name, value)]
  
  | Lstmt_function (name, params, body) ->
    let funcs, body = lift_expr body in
    funcs @ [Lstmt_function (name, params, body)]
  
  | Lstmt_external _ as stmt -> [stmt]

let lift_program program =
  List.map lift_stmt program
  |> List.flatten
