module Ident = Symbols.Ident

type ast = toplevel node list

and 'a node =
  {
    span: Span.t;
    item: 'a;
  }

(* ====================== Toplevel ===================== *)

and toplevel =
  | Ptl_value_def of value_definition
  | Ptl_type_def of type_definition
  | Ptl_extern_def of extern_definition
  | Ptl_import of import
  | Ptl_module of string node * ast

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
  | Pik_module
  | Pik_simple of string node list
  | Pik_glob
  | Pik_rename of string node list * string node
  | Pik_nested of import_kind node list
  
(* ====================== Pattern ====================== *)

(** TODO: add pattern matching like as in
    github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L219 *)
and pattern =
  | Ppat_wildcard
  | Ppat_variable of string
  | Ppat_literal of literal
  | Ppat_construct of Ident.t node * pattern node list
  | Ppat_tuple of pattern node list

(* ===================== Expression ===================== *)

and expr =
  | Pexp_grouping of expr node
  | Pexp_binding of value_binding * expr node
  | Pexp_lambda of value_binding
  | Pexp_match of expr node * case list
  | Pexp_switch of case list
  | Pexp_sequence of expr node * expr node
  | Pexp_constraint of expr node * typ node
  | Pexp_apply of expr node * expr node list
  | Pexp_tuple of expr node list
  | Pexp_construct of Ident.t node * expr node list
  | Pexp_literal of literal
  | Pexp_ident of Ident.t node
  | Pexp_magic of string
  | Pexp_error

and case = pattern node * expr node

and literal =
  | Pli_int of int
  | Pli_float of float
  | Pli_char of char
  | Pli_string of string
  | Pli_null

and value_binding =
  {
    vb_patt: pattern node;
    vb_expr: expr node;
  }

(* ======================== Type ======================== *)

and complex_typ =
  | Pct_variant of row list
  | Pct_simple of typ node

and row = Ident.t node * typ node list

and typ =
  (* generic *)
  | Pty_grouping of typ node
  | Pty_wildcard
  | Pty_forall of string node list * typ node
  | Pty_variable of string node
  | Pty_effect of typ node
  | Pty_function of typ node * typ node
  | Pty_tuple of typ node list
  | Pty_construct of typ node option * Ident.t node

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
