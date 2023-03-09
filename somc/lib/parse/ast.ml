module Ident = Symbols.Ident

type ast = toplevel node list

and 'a node =
  {
    span: Span.t;
    item: 'a;
  }

(* ====================== Toplevel ===================== *)

and toplevel =
  | TLValueDef of value_definition
  | TLTypeDef of type_definition
  | TLExternDef of extern_definition
  | TLImport of import
  | TLModule of string node * ast

and value_definition =
  {
    vd_name: Ident.t node;
    vd_expr: expr node;
  }

and type_definition =
  {
    td_params: string node list;
    td_name: Ident.t node;
    td_type: complex_typ node;
  }

and extern_definition =
  {
    (*
      the native name might be different but
      we do not keep track of the paramters
      as these are just for documentation.
    *)
    ed_native_name: string node;
    ed_name: Ident.t node;
    ed_type: typ node;
  }

(* ======================= Import ======================= *)

and import =
  {
    i_path: string node list;
    i_kind: import_kind node;
  }
    
and import_kind =
  | IK_Module
  | IK_Simple of string node list
  | IK_Glob
  | IK_Rename of string node list * string node
  | IK_Nested of import_kind node list
  
(* ====================== Pattern ====================== *)

(** TODO: add pattern matching like as in
    github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L219 *)
and pattern =
  | PAVariable of string
  | PAWildcard

(* ===================== Expression ===================== *)

and expr =
  | EXGrouping of expr node
  | EXBinding of value_binding * expr node
  | EXLambda of value_binding
  | EXSequence of expr node * expr node
  | EXConstraint of expr node * typ node
  | EXApplication of expr node * expr node list
  | EXTuple of expr node list
  | EXConstruct of Ident.t node * expr node list
  | EXLiteral of literal
  | EXIdentifier of Ident.t node
  | EXMagical of string
  | EXError

and literal =
  | LIInt of int
  | LIFloat of float
  | LIChar of char
  | LIString of string
  | LINil

and value_binding =
  {
    vb_patt: pattern node;
    vb_expr: expr node;
  }

(* ======================== Type ======================== *)

and complex_typ =
  | CTVariant of row list
  | CTSimple of typ node

and row = Ident.t node * typ node list

and typ =
  (* generic *)
  | TYGrouping of typ node
  | TYAny
  | TYForall of string node list * typ node
  | TYVariable of string node
  | TYEffect of typ node
  | TYFunction of typ node * typ node
  | TYTuple of typ node list
  | TYConstruct of typ node option * Ident.t node
  | TYPrimitive of primitive_typ

and primitive_typ =
  | PTInt of (bool * int) option
  | PTFloat of int option
  | PTVoid

(* ===================== Directive ===================== *)

(* and directive =
  {
    (* TODO: enum instead of string *)
    id: string node;
    arg: directive_arg node option;
  }

and directive_arg =
  | DA_Bool of bool
  | DA_Integer of int
  | DA_Float of float
  | DA_String of string
  | DA_Identifier of string
*)

let nmap n f = {n with item = f n.item}

let nmapi l = List.map (fun n -> n.item) l
let nmaps l = List.map (fun n -> n.span) l
