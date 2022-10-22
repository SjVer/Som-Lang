open Lsp.Types.Position
open Somc.Span
module Ast = Somc.Parse.Ast
module TAst = Somc.Typing.TAst

let pos_in_span pos span =
  let ch = pos.character + 1 in

  if span.start.line = span.end_.line then
    pos.line = span.start.line &&
    ch >= span.start.col &&
    ch <= span.end_.col
  else
    if pos.line >= span.start.line
    && pos.line <= span.end_.line then
      if pos.line = span.start.line then
        ch >= span.start.col
      else if pos.line = span.end_.line then
        ch <= span.end_.col
      else true
    else false

(* tast stuff *)

let find_tast_node pos tast =
  let open TAst in
  let is_in span = pos_in_span pos span in

  let rec go_patt (node: pattern tnode) =
    let this = `Pattern node in
    match node.item with
      | PA_Variable _
      | PA_Wildcard -> this
  in

  let rec go_expr (node: expr tnode) =
    let this = `Expression node in
    match node.item with
      | EX_Grouping e ->
        if is_in e.span then `Expression e
        else this
      | EX_Binding (b, e) ->
        if is_in b.patt.span then go_patt b.patt
        else if is_in b.expr.span then go_expr b.expr
        else if is_in e.span then go_expr e
        else this
      | EX_Lambda b ->
        if is_in b.patt.span then go_patt b.patt
        else if is_in b.expr.span then go_expr b.expr
        else this
      | EX_Sequence (e1, e2) ->
        if is_in e1.span then go_expr e1
        else if is_in e2.span then go_expr e2
        else this
      | EX_Application (e, es) ->
        if is_in e.span then go_expr e
        else let rec go = function
          | [] -> this
          | e :: es ->
            if is_in e.span then go_expr e
            else go es
        in go es
      | EX_Tuple es ->
        let rec go = function
          | [] -> this
          | e :: es ->
            if is_in e.span then go_expr e
            else go es
        in go es
      | EX_Construct (p, e) ->
        if is_in p.span then `String Ast.{item=p.typ; span=p.span}
        else if Option.is_some e then go_expr (Option.get e)
        else this
      | EX_Literal _
      | EX_Identifier _
      | EX_External _ -> this 
      | EX_Error -> `Not_found
  in

  let rec go_toplevel (node: toplevel node) =
    let this t = `Toplevel Ast.{item=t; span=node.span} in
    match node.item with
      | TL_Declaration (name, typ) ->
        if is_in name.span then `String {typ with span=name.span}
        else if is_in typ.span then `Type typ
        else this (Some typ.item)
      | TL_Definition {patt; expr} ->
        if is_in patt.span then `Pattern patt
        else if is_in expr.span then go_expr expr
        else this (Some expr.typ)
      | TL_Section (_, tast) ->
        match go tast with
          | `Not_found -> this None
          | node -> node

  and go = function
    | [] -> `Not_found
    | tl :: tls ->
      if pos_in_span pos tl.span then
        go_toplevel tl
      else
        go tls
  in go tast

let found_tast_node_type =
  let open TAst in function
    | `Toplevel o -> o.Ast.item
    | `Expression e -> Some e.typ
    | `Pattern p -> Some p.typ
    | `String t -> Some t.Ast.item
    | `Type t -> Some t.Ast.item
    | `Not_found -> None

let found_tast_node_span =
  let open TAst in function
    | `Toplevel n -> Some n.Ast.span
    | `Expression e -> Some e.span
    | `Pattern p -> Some p.span
    | `String t -> Some t.Ast.span
    | `Type t -> Some t.Ast.span
    | `Not_found -> None
