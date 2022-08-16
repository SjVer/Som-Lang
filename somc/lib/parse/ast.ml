type 'a node = {span: Span.span; item: 'a}

(** TODO: add pattern matching like as in
    github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L219 *)
and pattern =
  | Variable of string
  | Wildcard

and value_binding =
  {
    patt: pattern node;
    expr: expr node;
  }

and expr =
  | Grouping of expr node (** `(expr)` *)
  | Binding of value_binding list * expr node (** `pattern = expr, ... => expr` *)
  | Sequence of expr node * expr node (** `expr, expr` *)
  | BinaryOp of bin_op * expr node * expr node (** `expr op expr` *)
  | UnaryOp of un_op * expr node (** `op expr` *)
  | Literal of literal (** `literal` *)

and literal =
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | String of string
  
and bin_op =
  | Add (** `+` *)
  | Subtract (** `-` *)
  | Multiply (** `*` *)
  | Divide (** `/` *)
  | Power (** `^` *)

and un_op =
  | Negate (** `-` *)
  | Not (** `!` *)