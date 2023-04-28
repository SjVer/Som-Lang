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
  | Ttl_value_def of value_definition
  | Ttl_type_def of type_definition
  | Ttl_extern_def of extern_definition

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
  | Tpat_wildcard
  | Tpat_variable of string
  | Tpat_literal of literal
  | Tpat_construct of Ident.t tnode * pattern tnode list
  | Tpat_tuple of pattern tnode list

(* ===================== Expression ===================== *)

and expr =
  | Texp_grouping of expr tnode
  | Texp_binding of value_binding * expr tnode
  | Texp_lambda of value_binding
  | Texp_match of expr tnode * case list
  | Texp_switch of case list
  | Texp_if of expr tnode * expr tnode * expr tnode
  | Texp_sequence of expr tnode * expr tnode
  | Texp_apply of expr tnode * expr tnode list
  | Texp_tuple of expr tnode list
  | Texp_construct of Ident.t tnode * expr tnode list
  | Texp_literal of literal
  | Texp_ident of Ident.t tnode
  | Texp_magic of Symbols.Magic.t
  | Texp_error

and case = pattern tnode * expr tnode

and literal =
  | Tli_int of int
  | Tli_float of float
  | Tli_char of char
  | Tli_string of string
  | Tli_null

and value_binding =
  {
    vb_patt: pattern tnode;
    vb_expr: expr tnode;
  }
