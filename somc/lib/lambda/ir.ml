type ident = string

type const =
  | Lconst_int of int
  | Lconst_float of float
  | Lconst_string of string
  | Lconst_null
  [@@deriving show]

type var =
  | Lvar_local of ident
  | Lvar_global of ident
  | Lvar_tag of int

type prim = Symbols.Primitive.t

type atom =
  | Latom_const of const
  | Latom_var of var
  | Latom_prim of prim

type scrutinee = int list

and expr =
  | Lexpr_let of ident * expr * expr
  | Lexpr_lambda of ident list * expr
  | Lexpr_match of atom * (int * expr) list
  | Lexpr_call of atom * expr list
  | Lexpr_apply of expr * expr list
  | Lexpr_if of expr * expr * expr
  | Lexpr_sequence of expr * expr
  | Lexpr_tuple of atom list
  | Lexpr_object of int * atom list
  | Lexpr_lazy of expr (* unused right now *)
  | Lexpr_get of var * int
  | Lexpr_eval of var (* unused right now *)
  | Lexpr_atom of atom
  | Lexpr_fail

type statement =
  | Lstmt_definition of ident * expr
  | Lstmt_function of ident * ident list * expr
  | Lstmt_external of ident * ident * int

type program = statement list
