type 'a node = {span: Span.span; item: 'a}

and expr =
  | E_Grouping of expr node (** `(expr)` *)
  | E_Binding of value_binding list * expr node (** `pattern = expr, ... => expr` *)
  | E_Sequence of expr node * expr node (** `expr, expr` *)
  | E_Application of applicant node * expr node list (** `applicant expr ...` *)
  | E_Literal of literal (** `literal` *)
  | E_Ident of string (** `variable` TODO: allow shit like `Foo.bar` *)
  
and value_binding =
  {
    patt: pattern node;
    expr: expr node;
  }

(** TODO: add pattern matching like as in
    github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L219 *)
and pattern =
  | P_Variable of string
  | P_Wildcard

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
  