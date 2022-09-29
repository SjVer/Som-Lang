type 'a tnode =
  {
    span: Span.t;
    item: 'a;
    typ: Types.t;
  }

(* ====================== Toplevel ====================== *)

and value_binding =
  {
    patt: pattern tnode;
    expr: expr tnode;
  }

(* ====================== Pattern ======================= *)

and pattern =
  | PA_Variable of string
  | PA_Wildcard

(* ===================== Expression ===================== *)

and expr =
  | EX_Grouping of expr tnode
  | EX_Binding of value_binding list * expr tnode
  | EX_Lambda of value_binding
  | EX_Sequence of expr tnode * expr tnode
  | EX_Application of expr tnode * expr tnode list
  | EX_Tuple of expr tnode list
  | EX_Construct of Path.t tnode * expr tnode option
  | EX_Literal of literal
  | EX_Identifier of Path.t tnode

and literal =
  | LI_Int of int
  | LI_Float of float
  | LI_Char of char
  | LI_String of string
  | LI_Nil