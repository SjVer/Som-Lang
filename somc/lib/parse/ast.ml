type 'a node = {span: Span.span; item: 'a}

(* ====================== Toplevel ===================== *)

and toplevel =
  | TL_Declaration of string * typ node (** `string: typ` *)
  | TL_Definition of value_binding (** `patt = expr` *)

(* ====================== Pattern ====================== *)

(** TODO: add pattern matching like as in
    github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L219 *)
and pattern =
  | PA_Variable of string
  | PA_Wildcard

(* ===================== Expression ===================== *)

and expr =
  | EX_Grouping of expr node (** `(expr)` *)
  | EX_Binding of value_binding list * expr node (** `pattern = expr, ... => expr` *)
  | EX_Sequence of expr node * expr node (** `expr, expr` *)
  | EX_Application of applicant node * expr node list (** `applicant expr ...` *)
  (* | EX_Cast of expr node * typ node (** `expr -> typ` *) *)
  | EX_Literal of literal (** `literal` *)
  | EX_Ident of string (** `variable` TODO: allow shit like `Foo.bar` *)
  
and value_binding =
  {
    patt: pattern node;
    expr: expr node;
  }

and applicant =
  | AP_Expr of expr node
  | AP_BinaryOp of bin_op
  | AP_UnaryOp of un_op
  
and bin_op =
  | BI_Add
  | BI_Subtract
  | BI_Multiply
  | BI_Divide
  | BI_Power

and un_op =
  | UN_Negate
  | UN_Not

and literal =
  | LI_Bool of bool
  | LI_Int of int
  | LI_Float of float
  | LI_Char of char
  | LI_String of string

(* ======================== Type ======================== *)

and typ =
  | TY_Grouping of typ node (** `(typ)` *)
  | TY_Any (** `_` *)
  | TY_Var of string (** `'string` *)
  | TY_Effect of typ node option (** `!typ` *)
  | TY_Function of typ node list * typ node (** `typ, ... -> typ` *)
  | TY_Tuple of typ node list (** `typ; ...` *)
  | TY_List of typ node (** `[typ]` *)
  | TY_Constr of typ node option * string (** `string` | `typ string`*)
  | TY_Alias of typ node * string node (** `typ := 'string` *)
  | TY_Builtin of builtin_typ (** `$llvm_typ` *)

and builtin_typ =
  | BT_Int of bool * int (* `$i.bool.int` *)
  | BT_Float of int (* `$f.int` *)
  | BT_Void (* `$v` *)