open Ir

module SMap = Map.Make(String)

let rec convert_expr amap = function
  | Expr_let (name, value, expr) ->
    Expr_let (name,
      convert_expr amap value,
      convert_expr amap expr)

  | Expr_lambda _ -> invalid_arg "convert_expr Expr_lambda"

  | Expr_apply (Expr_atom (Atom_var (Var_global f)), args) as e ->
    let arity = match SMap.find_opt f amap with
      | Some arity -> arity
      | None -> 0
    in
    if List.length args = arity then
      Expr_call (Atom_var (Var_global f), args)
    else if List.length args > arity then
      let cargs, aargs = Lower.cut arity args in
      let call = Expr_call (Atom_var (Var_global f), cargs) in
      Expr_apply (call, aargs)
    else
      e

  | expr -> expr

let convert_stmt amap = function
  | Stmt_definition (name, expr) ->
    amap, Stmt_definition (name, convert_expr amap expr)
  | Stmt_function (name, params, body) ->
    let amap = SMap.add name (List.length params) amap in
    amap, Stmt_function (name, params, convert_expr amap body)
  | Stmt_external (name, nname) ->
    amap, Stmt_external (name, nname)

let convert_applications program =
  let rec go amap program = function
    | [] -> program
    | stmt :: stmts ->
      let amap, stmt = convert_stmt amap stmt in
      go amap (program @ [stmt]) stmts
  in
  go SMap.empty [] program
