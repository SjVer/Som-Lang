type ident = string

type const =
  | Const_int of int
  | Const_float of float
  | Const_string of string
  | Const_nil
  [@@deriving show]

type var =
  | Var_local of ident
  | Var_global of ident
  | Var_tag of int

type atom =
  | Atom_const of const
  | Atom_var of var
  | Atom_magic of Symbols.Magic.t

type scrutinee = int list

and expr =
  | Expr_let of ident * expr * expr
  | Expr_lambda of ident list * expr
  | Expr_match of atom * (int * expr) list
  | Expr_call of atom * atom list
  | Expr_apply of expr * atom list
  | Expr_if of expr * expr * expr
  | Expr_sequence of expr * expr
  | Expr_tuple of atom list
  | Expr_object of int * atom list
  | Expr_lazy of expr (* unused right now *)
  | Expr_get of var * int
  | Expr_eval of var
  | Expr_atom of atom
  | Expr_fail

type statement =
  | Stmt_definition of ident * expr
  | Stmt_external of ident * ident

type program = statement list