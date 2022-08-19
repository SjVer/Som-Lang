type 'a node = {span: Span.span; item: 'a}

(* ====================== Pattern ====================== *)

(** TODO: add pattern matching like as in
    github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L219 *)
and pattern =
  | P_Variable of string
  | P_Wildcard

(* ===================== Expression ===================== *)

and expr =
  | E_Grouping of expr node (** `(expr)` *)
  | E_Binding of value_binding list * expr node (** `pattern = expr, ... => expr` *)
  | E_Sequence of expr node * expr node (** `expr, expr` *)
  | E_Application of applicant node * expr node list (** `applicant expr ...` *)
  | E_Cast of expr node * typ node (** `expr -> typ` *)
  | E_Literal of literal (** `literal` *)
  | E_Ident of string (** `variable` TODO: allow shit like `Foo.bar` *)
  
and value_binding =
  {
    patt: pattern node;
    expr: expr node;
  }

and applicant =
  | A_Expr of expr node
  | A_BinaryOp of bin_op
  | A_UnaryOp of un_op
  
and bin_op =
  | B_Add
  | B_Subtract
  | B_Multiply
  | B_Divide
  | B_Power

and un_op =
  | U_Negate
  | U_Not

and literal =
  | L_Bool of bool
  | L_Int of int
  | L_Float of float
  | L_Char of char
  | L_String of string

(* ======================== Type ======================== *)

and typ =
  | T_Grouping of typ node (** `(typ)` *)
  | T_Any (** `_` *)
  | T_Var of string (** `'string` *)
  | T_Function of typ node * typ node (** `typ -> typ` *)
  | T_Tuple of typ node list (** `(typ; list; ...)` *)
  | T_Constr of string * typ node list (** `string` | `(typ, list, ...) string`*)
  | T_Alias of typ node * string node (** `typ := 'string` *)
  | T_Builtin of builtin_typ (** `$llvm_typ` *)

and builtin_typ =
  | T_B_Int of bool * int (* `$i.bool.int` *)
  | T_B_Float of int (* `$f.int` *)
  | T_B_Void (* `$v` *)