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
      | None -> Int.max_int (* TODO: wrong? *)
    in
    if List.length args = arity then
      Expr_call (Atom_var (Var_global f), args)
    else if List.length args > arity then
      let cargs, aargs = Lower.cut arity args in
      let call = Expr_call (Atom_var (Var_global f), cargs) in
      Expr_apply (call, aargs)
    else
      e

  | Expr_apply (f, args) ->
    Expr_apply (convert_expr amap f, args) 

  | Expr_match (scrut, cases) ->
    let do_case (int, expr) = int, convert_expr amap expr in
    let cases = List.map do_case cases in
    Expr_match (scrut, cases)

  | Expr_if (cond, iexpr, eexpr) ->
    Expr_if (
      convert_expr amap cond,
      convert_expr amap iexpr,
      convert_expr amap eexpr)

  | Expr_sequence (lexpr, rexpr) ->
    Expr_sequence (
      convert_expr amap lexpr,
      convert_expr amap rexpr)

  | Expr_lazy expr ->
    Expr_lazy (convert_expr amap expr)

  | expr -> expr

let convert_stmt amap = function
  | Stmt_definition (name, expr) ->
    amap, Stmt_definition (name, convert_expr amap expr)
  | Stmt_function (name, params, body) ->
    let amap = SMap.add name (List.length params) amap in
    amap, Stmt_function (name, params, convert_expr amap body)
  | Stmt_external (name, _, arity) as stmt ->
    SMap.add name arity amap, stmt

let rec convert_statements = function
  | [] -> []
  | Stmt_definition (name, Expr_lambda (params, body)) :: stmts ->
    let stmt = Stmt_function (name, params, body) in
    convert_statements (stmt :: stmts)
  | Stmt_function (name, fparams, Expr_lambda (lparams, body)) :: stmts ->
    let stmt = Stmt_function (name, fparams @ lparams, body) in
    convert_statements (stmt :: stmts)
  | stmt :: stmts ->
    stmt :: convert_statements stmts

let convert_program program =
  let rec go amap program = function
    | [] -> program
    | stmt :: stmts ->
      let amap, stmt = convert_stmt amap stmt in
      go amap (program @ [stmt]) stmts
  in
  go SMap.empty [] program
