type ident = string

type const =
  | Const_int of int
  | Const_float of float
  | Const_string of string
  | Const_null
  [@@deriving show]

type var =
  | Var_local of ident
  | Var_global of ident
  | Var_tag of int

type prim = Symbols.Primitive.t

type atom =
  | Atom_const of const
  | Atom_var of var
  | Atom_prim of prim

type scrutinee = int list

and expr =
  | Expr_let of ident * expr * expr
  | Expr_lambda of ident list * expr
  | Expr_match of atom * (int * expr) list
  | Expr_call of atom * expr list
  | Expr_apply of expr * expr list
  | Expr_if of expr * expr * expr
  | Expr_sequence of expr * expr
  | Expr_tuple of atom list
  | Expr_object of int * atom list
  | Expr_lazy of expr (* unused right now *)
  | Expr_get of var * int
  | Expr_eval of var (* unused right now *)
  | Expr_atom of atom
  | Expr_fail

type statement =
  | Stmt_definition of ident * expr
  | Stmt_function of ident * ident list * expr
  | Stmt_external of ident * ident * int

type program = statement list
