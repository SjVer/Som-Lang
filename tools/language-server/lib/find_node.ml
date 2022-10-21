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

  let rec find_in_expr (node: expr tnode) =
    match node.item with
      | EX_Grouping e when is_in e.span -> find_in_expr e
      | _ -> `Expression node
  in

  let rec find_in_toplevel (node: toplevel node) =
    match node.item with
      | TL_Declaration (name, typ) ->
        if is_in name.span then `String typ
        else if is_in typ.span then `Type typ
        else `Toplevel node
      | TL_Definition {patt; expr} ->
        if is_in patt.span then `Pattern patt
        else if is_in expr.span then find_in_expr expr
        else `Toplevel node
      | TL_Section (_, tast) ->
        match go tast with
          | `Not_found -> `Toplevel node
          | node -> node

  and go = function
    | [] -> `Not_found
    | tl :: tls ->
      if pos_in_span pos tl.span then
        find_in_toplevel tl
      else
        go tls
  in go tast

let found_tast_node_type =
  let open TAst in function
    | `Expression e -> Some e.typ
    | `Pattern p -> Some p.typ
    | `String t -> Some t.Ast.item
    | `Toplevel _ -> None
    | `Type t -> Some t.Ast.item
    | `Not_found -> None

let found_tast_node_span =
  let open TAst in function
    | `Expression e -> Some e.span
    | `Pattern p -> Some p.span
    | `String t -> Some t.Ast.span
    | `Toplevel n -> Some n.Ast.span
    | `Type t -> Some t.Ast.span
    | `Not_found -> None
