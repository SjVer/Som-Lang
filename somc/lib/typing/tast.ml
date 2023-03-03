module Ident = Symbols.Ident

type tast = toplevel node list

and 'a node = 'a Parse.Ast.node

and 'a tnode =
  {
    span: Span.t;
    item: 'a;
    typ: Types.t;
  }

(* ====================== Toplevel ====================== *)

and toplevel =
  | TL_Value_Definition of value_definition
  | TL_Type_Definition of type_definition

and value_definition =
  {
    vd_name: Ident.t node;
    vd_expr: expr tnode;
  }

and type_definition =
  {
    td_params: string node list;
    td_name: Ident.t node;
    td_type: Types.t node;
  }
  

(* ====================== Pattern ======================= *)

and pattern =
  | PA_Variable of string
  | PA_Wildcard

(* ===================== Expression ===================== *)

and expr =
  | EX_Grouping of expr tnode
  | EX_Binding of value_binding * expr tnode
  | EX_Lambda of value_binding
  | EX_Sequence of expr tnode * expr tnode
  | EX_Application of expr tnode * expr tnode list
  | EX_Tuple of expr tnode list
  | EX_Construct of Symbols.Ident.t tnode * expr tnode option
  | EX_Literal of literal
  | EX_Identifier of Symbols.Ident.t tnode
  | EX_External of string
  | EX_Error

and literal =
  | LI_Int of int
  | LI_Float of float
  | LI_Char of char
  | LI_String of string
  | LI_Nil

and value_binding =
  {
    vb_patt: pattern tnode;
    vb_expr: expr tnode;
  }