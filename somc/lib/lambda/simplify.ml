open Ir

let rec _make_opt f program =
  let program' = f program in
  if program' = program then program'
  else _make_opt f program'

module LetAlias = struct
  (* simplifies `(let x y in (..x..)`) to `..y..` *)

  let replaced : (ident, atom) Hashtbl.t = Hashtbl.create 10

  let check_atom = function
    | Latom_var (Lvar_local var) as atom -> begin
        try Hashtbl.find replaced var
        with Not_found -> atom
      end
    | atom -> atom

  let rec simplify_expr = function
    | Lexpr_let (var, Lexpr_atom a, expr) ->
      let expr = simplify_expr expr in
      Hashtbl.add replaced var a;
      simplify_expr expr
    
    | Lexpr_let (var, value, expr) ->
      Lexpr_let (var, simplify_expr value, simplify_expr expr)
    | Lexpr_lambda (args, expr) ->
      Lexpr_lambda (args, simplify_expr expr)
    | Lexpr_match (s, cases) ->
      let f (tag, expr) = tag, simplify_expr expr in
      Lexpr_match (s, List.map f cases)
    | Lexpr_call (f, args) ->
      Lexpr_call (check_atom f, List.map simplify_expr args)
    | Lexpr_apply (f, args) ->
      Lexpr_apply (simplify_expr f, List.map simplify_expr args)
    | Lexpr_if (cond, texpr, eexpr) ->
      Lexpr_if (simplify_expr cond,
        simplify_expr texpr,
        simplify_expr eexpr)
    | Lexpr_sequence (e1, e2) ->
      Lexpr_sequence (simplify_expr e1, simplify_expr e2)
    | Lexpr_lazy expr ->
      Lexpr_lazy (simplify_expr expr)
    | Lexpr_tuple els ->
      Lexpr_tuple (List.map check_atom els)
    | Lexpr_object (tag, els) ->
      Lexpr_object (tag, List.map check_atom els)
    | Lexpr_eval (Lvar_local var | Lvar_global var) as expr -> begin
        try Lexpr_atom (Hashtbl.find replaced var)
        with Not_found -> expr
      end
    | Lexpr_atom a -> Lexpr_atom (check_atom a)
    | expr -> expr

  let simplify_stmt = function
    | Lstmt_definition (name, expr) ->
      Hashtbl.reset replaced;
      Lstmt_definition (name, simplify_expr expr)
    | Lstmt_function (name, params, expr) ->
      Hashtbl.reset replaced;
      Lstmt_function (name, params, simplify_expr expr)
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
    | Lexpr_let (
        var1, Lexpr_apply (f, args1),
        Lexpr_apply (Lexpr_atom (Latom_var (Lvar_local var2)), args2))
      when var1 = var2 -> Lexpr_apply (f, args1 @ args2)

    | Lexpr_let (var, value, expr) ->
      Lexpr_let (var, simplify_expr value, simplify_expr expr)
    | Lexpr_lambda (args, expr) ->
      Lexpr_lambda (args, simplify_expr expr)
    | Lexpr_match (s, cases) ->
      let f (tag, expr) = tag, simplify_expr expr in
      Lexpr_match (s, List.map f cases)
    | Lexpr_apply (f, args) ->
      Lexpr_apply (simplify_expr f, args)
    | Lexpr_if (cond, texpr, eexpr) ->
      Lexpr_if (
        simplify_expr cond,
        simplify_expr texpr,
        simplify_expr eexpr)
    | Lexpr_sequence (e1, e2) ->
      Lexpr_sequence (simplify_expr e1, simplify_expr e2)
    | Lexpr_lazy expr ->
      Lexpr_lazy (simplify_expr expr)
    | expr -> expr

  let simplify_stmt = function
    | Lstmt_definition (name, expr) ->
      Lstmt_definition (name, simplify_expr expr)
    | Lstmt_function (name, params, expr) ->
      Lstmt_function (name, params, simplify_expr expr)
    | stmt -> stmt

  let simplify_program =
    _make_opt (List.map simplify_stmt) 
end

module Uncurry = struct
  let rec uncurry_expr = function
    | Lexpr_lambda (params, (Lexpr_lambda _ as lam)) ->
      let [@ warning "-8"] Lexpr_lambda (params', body) =
        uncurry_expr lam
      in
      Lexpr_lambda (params @ params', body)
    | expr -> expr

  let uncurry_stmt = function
    | Lstmt_definition (name, value) ->
      Lstmt_definition (name, uncurry_expr value)
    | Lstmt_function (name, params, value) ->
      Lstmt_function (name, params, uncurry_expr value)
    | stmt -> stmt

  let uncurry_program = List.map uncurry_stmt
end

let basic_simplify_pogram =
  _make_opt (fun prog -> prog
    |> LetAlias.simplify_program
    |> NestedApply.simplify_program)
