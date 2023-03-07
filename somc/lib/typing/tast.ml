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
  | TLValueDef of value_definition
  | TLTypeDef of type_definition
  | TLExternDef of extern_definition

and value_definition =
  {
    vd_name: Ident.t node;
    vd_expr: expr tnode;
  }

and type_definition =
  {
    td_name: Ident.t node;
    td_type: Types.t node;
  }

and extern_definition =
  {
    ed_native_name: string node;
    ed_name: Ident.t node;
    ed_type: Types.t node;
  }
  

(* ====================== Pattern ======================= *)

and pattern =
  | PAVariable of string
  | PAWildcard

(* ===================== Expression ===================== *)

and expr =
  | EXGrouping of expr tnode
  | EXBinding of value_binding * expr tnode
  | EXLambda of value_binding
  | EXSequence of expr tnode * expr tnode
  | EXApplication of expr tnode * expr tnode list
  | EXTuple of expr tnode list
  | EXConstruct of Symbols.Ident.t tnode * expr tnode list
  | EXLiteral of literal
  | EXIdentifier of Symbols.Ident.t tnode
  | EXMagical of Magicals.t
  | EXError

and literal =
  | LIInt of int
  | LIFloat of float
  | LIChar of char
  | LIString of string
  | LINil

and value_binding =
  {
    vb_patt: pattern tnode;
    vb_expr: expr tnode;
  }