type ident = string

type const =
  | Const_int of int
  | Const_float of float
  | Const_string of string
  | Const_nil

type var =
  | Var_local of ident
  | Var_global of ident

type atom =
  | Atom_const of const
  | Atom_var of var

type expr =
  | Expr_lambda of (ident list * expr) step
  | Expr_apply of (var * atom list) step
  | Expr_eval of var step
  | Expr_if of (atom * expr * expr) step
  | Expr_sequence of (expr * expr) step
  | Expr_tuple of (atom list) step
  | Expr_atom of atom
  | Expr_get of (var * int) step

and 'a step = ident * 'a * expr
(* let [string] = ['a] in [expr] *)

type statement =
  | Stmt_definition of ident * expr
  | Stmt_external of ident * ident

type program = statement list