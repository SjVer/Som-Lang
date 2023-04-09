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
  | Expr_let of ident * expr * expr
  | Expr_lambda of ident list * expr
  | Expr_apply of atom * atom list
  | Expr_eval of var
  | Expr_if of atom * expr * expr
  | Expr_sequence of expr * expr
  | Expr_tuple of atom list
  | Expr_atom of atom
  | Expr_get of var * int

(* and 'a step = ident * 'a * expr *)
(* let [string] = ['a] in [expr] *)

type statement =
  | Stmt_definition of ident * expr
  | Stmt_external of ident * ident

type program = statement list