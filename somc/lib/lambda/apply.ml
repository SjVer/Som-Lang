open Ir

module SMap = Map.Make(String)

let rec convert_expr amap = function
  | Lexpr_let (name, value, expr) ->
    Lexpr_let (name,
      convert_expr amap value,
      convert_expr amap expr)

  | Lexpr_lambda _ -> invalid_arg "convert_expr Lexpr_lambda"

  | Lexpr_apply (Lexpr_atom (Latom_var (Lvar_global f)), args) as e ->
    let arity = match SMap.find_opt f amap with
      | Some arity -> arity
      | None -> Int.max_int (* TODO: wrong? *)
    in
    if List.length args = arity then
      Lexpr_call (Latom_var (Lvar_global f), args)
    else if List.length args > arity then
      let cargs, aargs = Lower.cut arity args in
      let call = Lexpr_call (Latom_var (Lvar_global f), cargs) in
      Lexpr_apply (call, aargs)
    else
      e

  | Lexpr_apply (f, args) ->
    Lexpr_apply (convert_expr amap f, args) 

  | Lexpr_match (scrut, cases) ->
    let do_case (int, expr) = int, convert_expr amap expr in
    let cases = List.map do_case cases in
    Lexpr_match (scrut, cases)

  | Lexpr_if (cond, iexpr, eexpr) ->
    Lexpr_if (
      convert_expr amap cond,
      convert_expr amap iexpr,
      convert_expr amap eexpr)

  | Lexpr_sequence (lexpr, rexpr) ->
    Lexpr_sequence (
      convert_expr amap lexpr,
      convert_expr amap rexpr)

  | Lexpr_lazy expr ->
    Lexpr_lazy (convert_expr amap expr)

  | expr -> expr

let convert_stmt amap = function
  | Lstmt_definition (name, expr) ->
    amap, Lstmt_definition (name, convert_expr amap expr)
  | Lstmt_function (name, params, body) ->
    let amap = SMap.add name (List.length params) amap in
    amap, Lstmt_function (name, params, convert_expr amap body)
  | Lstmt_external (name, _, arity) as stmt ->
    SMap.add name arity amap, stmt

let rec convert_statements = function
  | [] -> []
  | Lstmt_definition (name, Lexpr_lambda (params, body)) :: stmts ->
    let stmt = Lstmt_function (name, params, body) in
    convert_statements (stmt :: stmts)
  | Lstmt_function (name, fparams, Lexpr_lambda (lparams, body)) :: stmts ->
    let stmt = Lstmt_function (name, fparams @ lparams, body) in
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
