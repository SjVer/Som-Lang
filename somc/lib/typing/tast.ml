type 'a node =
  {
    span: Span.span;
    item: 'a;
    typ: Types.type_expr;
  }

(* ====================== Toplevel ====================== *)

and value_binding =
  {
    (* patt: pattern node; *)
    expr: expr node;
  }

(* ===================== Expression ===================== *)

and expr =
  | EX_Grouping of expr node
  | EX_Binding of value_binding list * expr node
  | EX_Lambda of value_binding
  (* | EX_Sequence of expr node * expr node *)
  | EX_Application of applicant node * expr node list
  | EX_Tuple of expr node list
  | EX_Construct of Ident.t node * expr node option
  | EX_Literal of literal
  | EX_Identifier of Ident.t node

and applicant =
  | 