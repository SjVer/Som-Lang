open Tast
open ANSITerminal

let p i str typ span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Cyan] (" : " ^ Types.show_type typ);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()

(** show functions *)

let show_literal = function
  | LI_Int i -> "Int " ^ string_of_int i
  | LI_Float f -> "Float " ^ string_of_float f
  | LI_Char c -> "Char '" ^ String.make 1 c ^ "'"
  | LI_String s -> "String \"" ^ String.escaped s ^ "\""
  | LI_Nil -> "Nil"

(* print functions *)

let rec print_patt_node' i node =
  let {span; item; typ} = node in
  match item with
    | PA_Variable n ->
      p i ("PA_Variable " ^ n) typ span;
    | PA_Wildcard ->
      p i "PA_Wildcard" typ span

and print_expr_node i node =
  let {span; item; typ} = node in
  match item with
    | EX_Grouping e -> 
      p i "EX_Grouping" typ span;
      print_expr_node (i + 1) e
    | EX_Binding (bind, e) ->
      p i "EX_Binding" typ span;
      print_patt_node' (i + 1) bind.patt;
      print_expr_node (i + 2) bind.expr;
      print_expr_node (i + 1) e
    | EX_Lambda {patt; expr} ->
      p i "EX_Lambda" typ span;
      print_patt_node' (i + 1) patt;
      print_expr_node (i + 1) expr
    | EX_Sequence (e1, e2) ->
      p i "EX_Sequence" typ span;
      print_expr_node (i + 1) e1;
      print_expr_node (i + 1) e2
    | EX_Application (a, es) ->
      p i "EX_Application" typ span;
      print_expr_node (i + 1) a;
      List.iter (print_expr_node (i + 1)) es
    | EX_Tuple es ->
      p i "EX_Tuple" typ span;
      List.iter (print_expr_node (i + 1)) es
    | EX_Construct (n, e) ->
      p i ("EX_Construct " ^ Path.to_string n.item) typ span;
      if Option.is_some e
      then print_expr_node (i + 1) (Option.get e)
    | EX_Literal l ->
      p i ("EX_Literal " ^ show_literal l) typ span
    | EX_Identifier {span=_; item=id; typ=_} ->
      p i ("EX_Identifier " ^ Path.to_string id) typ span

(* and print_toplevel_node i node =
  let {span; item; typ} = node in
  match item with
    | TL_Declaration (n, t) ->
      p i ("TL_Declaration " ^ n) span;
      print_type_node (i + 1) t
    
    | TL_Definition { patt; expr } ->
      p i "TL_Definition" span;
      print_patt_node' (i + 1) patt;
      print_expr_node (i + 1) expr

    | TL_Type_Definition d ->
      let rec join = function
      | [] -> ""
      | v :: vs -> "'" ^ v.item ^ " " ^ join vs
      in let name = join d.params ^ d.name.item in
      p i ("TL_Type_Definition " ^ name) span;
      print_type_node (i + 1) d.typ

    | TL_Import {path; kind} ->
      p i ("TL_Import " ^ show_path path) span;
      print_import_kind_node' (i + 1) kind *)

(* expose functions *)

let print_expr_node = print_expr_node 0
(* let print_toplevel_node = print_toplevel_node 0 *)

(* let rec print_toplevel nodes =
  match nodes with
  | [] -> ()
  | [n] -> print_toplevel_node n
  | n :: ns ->
    print_toplevel_node n;
    print_newline ();
    print_toplevel ns *)