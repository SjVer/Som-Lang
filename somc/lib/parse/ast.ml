type ast = toplevel node list

and 'a node =
  {
    span: Span.t;
    item: 'a;
  }

(* ====================== Toplevel ===================== *)

and toplevel =
  | TL_Definition of value_binding
  | TL_Type_Definition of type_definition
  | TL_Import of import
  | TL_Module of string node * ast

and value_binding =
  {
    patt: pattern node;
    expr: expr node;
  }

and type_definition =
  {
    params: string node list;
    name: string node;
    typ: typ node;
  }
  
(* ======================= Import ======================= *)

and import =
  {
    path: string node list;
    kind: import_kind node;
  } 
    
and import_kind =
  | IK_Simple of string node
  | IK_Glob
  | IK_Rename of string node * string node
  | IK_Nested of import node list
  
(* ====================== Pattern ====================== *)

(** TODO: add pattern matching like as in
    github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L219 *)
and pattern =
  | PA_Variable of string
  | PA_Wildcard

(* ===================== Expression ===================== *)

and expr =
  | EX_Grouping of expr node
  | EX_Binding of value_binding * expr node
  | EX_Lambda of value_binding
  | EX_Sequence of expr node * expr node
  | EX_Constraint of expr node * typ node
  | EX_Application of expr node * expr node list
  | EX_Tuple of expr node list
  | EX_Construct of Ident.t node * expr node list
  | EX_Literal of literal
  | EX_Identifier of Ident.t node
  | EX_External of string
  | EX_Error

and literal =
  | LI_Int of int
  | LI_Float of float
  | LI_Char of char
  | LI_String of string
  | LI_Nil

(* ======================== Type ======================== *)

and typ =
  (* typedef-only *)
  | TY_Variant of (string node * typ node list) list
  (* generic *)
  | TY_Grouping of typ node
  | TY_Any
  | TY_Variable of string
  | TY_Effect of typ node
  | TY_Function of typ node * typ node
  | TY_Tuple of typ node list
  | TY_Construct of typ node option * Ident.t node
  | TY_Primitive of primitive_typ

and primitive_typ =
  | PT_Int of (bool * int) option
  | PT_Float of int option
  | PT_Void

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

let nmapi l = List.map (fun n -> n.item) l
let nmaps l = List.map (fun n -> n.span) l