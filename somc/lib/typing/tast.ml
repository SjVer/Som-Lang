type 'a node =
  {
    span: Span.span;
    item: 'a;
    typ: Types.type_expr;
  }

(* ====================== Toplevel ====================== *)

and value_binding =
  {
    patt: pattern node;
    expr: expr node;
  }

(* ====================== Pattern ======================= *)

and pattern =
  | PA_Variable of string
  | PA_Wildcard

(* ===================== Expression ===================== *)

and expr =
  | EX_Grouping of expr node
  | EX_Binding of value_binding list * expr node
  | EX_Lambda of value_binding
  (* | EX_Sequence of expr node * expr node *)
  | EX_Application of expr node * expr node list
  | EX_Tuple of expr node list
  | EX_Construct of Path.t node * expr node option
  | EX_Literal of literal
  | EX_Identifier of Path.t node

and literal =
  | LI_Bool of bool
  | LI_Int of int
  | LI_Float of float
  | LI_Char of char
  | LI_String of string
  | LI_Nil