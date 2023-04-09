open Ir


module RemoveLetAtom = struct
  let replaced : (ident, atom) Hashtbl.t = Hashtbl.create 10

  let check_atom = function
    | Atom_var (Var_local var) as atom -> begin
        try Hashtbl.find replaced var
        with Not_found -> atom
      end
    | atom -> atom

  let rec optimize_expr = function
    | Expr_let (var, Expr_atom a, expr) ->
      Hashtbl.add replaced var a;
      optimize_expr expr
    
    | Expr_let (var, value, expr) ->
      Expr_let (var, optimize_expr value, optimize_expr expr)
    | Expr_lambda (args, expr) ->
      Expr_lambda (args, optimize_expr expr)
    | Expr_apply (f, args) ->
      Expr_apply (check_atom f, List.map check_atom args)
    | Expr_if (cond, thenexpr, elseexpr) ->
      Expr_if (check_atom cond,
        optimize_expr thenexpr,
        optimize_expr elseexpr)
    | Expr_sequence (e1, e2) ->
      Expr_sequence (optimize_expr e1, optimize_expr e2)
    | Expr_lazy expr ->
      Expr_lazy (optimize_expr expr)
    | Expr_tuple els ->
      Expr_tuple (List.map check_atom els)
    | Expr_eval (Var_local var) as expr -> begin
        try Expr_atom (Hashtbl.find replaced var)
        with Not_found -> expr
      end
    | Expr_atom a -> Expr_atom (check_atom a)
    | expr -> expr

  let optimize_stmt = function
    | Stmt_definition (i, expr) ->
      Hashtbl.reset replaced;
      Stmt_definition (i, optimize_expr expr)
    | stmt -> stmt
end

let optimize_stmt stmt =
  RemoveLetAtom.optimize_stmt stmt

let rec optimize program =
  let program' = List.map optimize_stmt program in
  if program' = program then program'
  else optimize program'