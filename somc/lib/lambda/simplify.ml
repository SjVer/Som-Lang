open Ir

let rec _make_opt f program =
  let program' = f program in
  if program' = program then program'
  else _make_opt f program'

module LetAlias = struct
  (* simplifies `(let x y in (..x..)`) to `..y..` *)

  let replaced : (ident, atom) Hashtbl.t = Hashtbl.create 10

  let check_atom = function
    | Atom_var (Var_local var) as atom -> begin
        try Hashtbl.find replaced var
        with Not_found -> atom
      end
    | atom -> atom

  let rec simplify_expr = function
    | Expr_let (var, Expr_atom a, expr) ->
      let expr = simplify_expr expr in
      Hashtbl.add replaced var a;
      simplify_expr expr
    
    | Expr_let (var, value, expr) ->
      Expr_let (var, simplify_expr value, simplify_expr expr)
    | Expr_lambda (args, expr) ->
      Expr_lambda (args, simplify_expr expr)
    | Expr_match (s, cases) ->
      let f (tag, expr) = tag, simplify_expr expr in
      Expr_match (s, List.map f cases)
    | Expr_call (f, args) ->
      Expr_call (check_atom f, List.map check_atom args)
    | Expr_apply (f, args) ->
      Expr_apply (simplify_expr f, List.map check_atom args)
    | Expr_if (cond, texpr, eexpr) ->
      Expr_if (check_atom cond,
        simplify_expr texpr,
        simplify_expr eexpr)
    | Expr_sequence (e1, e2) ->
      Expr_sequence (simplify_expr e1, simplify_expr e2)
    | Expr_lazy expr ->
      Expr_lazy (simplify_expr expr)
    | Expr_tuple els ->
      Expr_tuple (List.map check_atom els)
    | Expr_object (tag, els) ->
      Expr_object (tag, List.map check_atom els)
    | Expr_eval (Var_local var | Var_global var) as expr -> begin
        try Expr_atom (Hashtbl.find replaced var)
        with Not_found -> expr
      end
    | Expr_atom a -> Expr_atom (check_atom a)
    | expr -> expr

  let simplify_stmt = function
    | Stmt_definition (i, expr) ->
      Hashtbl.reset replaced;
      Stmt_definition (i, simplify_expr expr)
    | stmt -> stmt

  let simplify_program =
    _make_opt (List.map simplify_stmt) 
end

module NestedApply = struct
  (*
    simplifies nested applications like
      `(let r (apply f ...) in (apply r ...))`
    into
      `(apply f ... ...)`
  *)

  let rec simplify_expr = function
    | Expr_let (
        var1, Expr_apply (f, args1),
        Expr_apply (Expr_atom (Atom_var (Var_local var2)), args2))
    when var1 = var2 ->
      Expr_apply (f, args1 @ args2)

    | Expr_let (var, value, expr) ->
      Expr_let (var, simplify_expr value, simplify_expr expr)
    | Expr_lambda (args, expr) ->
      Expr_lambda (args, simplify_expr expr)
    | Expr_match (s, cases) ->
      let f (tag, expr) = tag, simplify_expr expr in
      Expr_match (s, List.map f cases)
    | Expr_apply (f, args) ->
      Expr_apply (simplify_expr f, args)
    | Expr_if (cond, texpr, eexpr) ->
      Expr_if (cond, simplify_expr texpr, simplify_expr eexpr)
    | Expr_sequence (e1, e2) ->
      Expr_sequence (simplify_expr e1, simplify_expr e2)
    | Expr_lazy expr ->
      Expr_lazy (simplify_expr expr)
    | expr -> expr

  let simplify_stmt = function
    | Stmt_definition (i, expr) ->
      Stmt_definition (i, simplify_expr expr)
    | stmt -> stmt

  let simplify_program =
    _make_opt (List.map simplify_stmt) 
end

module ApplyToCall = struct
  (* replaces stuff like `(apply #add ...)` to `(call #add ...)` *)

  let rec simplify_expr = function
    | Expr_let (var, value, expr) ->
      Expr_let (var, simplify_expr value, simplify_expr expr)
    | Expr_lambda (args, expr) ->
      Expr_lambda (args, simplify_expr expr)
    | Expr_match (s, cases) ->
      let f (tag, expr) = tag, simplify_expr expr in
      Expr_match (s, List.map f cases)

    | Expr_apply (Expr_atom (Atom_magic m), args) ->
      Expr_call (Atom_magic m, args)

    | Expr_apply (f, args) ->
      Expr_apply (simplify_expr f, args)
    | Expr_if (cond, texpr, eexpr) ->
      Expr_if (cond, simplify_expr texpr, simplify_expr eexpr)
    | Expr_sequence (e1, e2) ->
      Expr_sequence (simplify_expr e1, simplify_expr e2)
    | Expr_lazy expr ->
      Expr_lazy (simplify_expr expr)
    | expr -> expr

  let simplify_stmt = function
    | Stmt_definition (i, expr) ->
      Stmt_definition (i, simplify_expr expr)
    | stmt -> stmt

  let simplify_program =
    _make_opt (List.map simplify_stmt) 
end

let basic_simplify_pogram =
  _make_opt (fun prog -> prog
    |> LetAlias.simplify_program
    |> NestedApply.simplify_program)