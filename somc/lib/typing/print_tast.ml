open Tast
open ANSITerminal

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()
let pt i str typ span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Cyan] (" : " ^ Types.show typ true);
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
      pt i ("PA_Variable " ^ n) typ span;
    | PA_Wildcard ->
      pt i "PA_Wildcard" typ span

and print_expr_node i node =
  let {span; item; typ} = node in
  match item with
    | EX_Grouping e -> 
      pt i "EX_Grouping" typ span;
      print_expr_node (i + 1) e
    
    | EX_Binding (bind, e) ->
      pt i "EX_Binding" typ span;
      print_patt_node' (i + 1) bind.patt;
      print_expr_node (i + 1) bind.expr;
      print_expr_node (i + 1) e
  
    | EX_Lambda {patt; expr} ->
      pt i "EX_Lambda" typ span;
      print_patt_node' (i + 1) patt;
      print_expr_node (i + 1) expr
  
    | EX_Sequence (e1, e2) ->
      pt i "EX_Sequence" typ span;
      print_expr_node (i + 1) e1;
      print_expr_node (i + 1) e2
    
    | EX_Application (a, es) ->
      pt i "EX_Application" typ span;
      print_expr_node (i + 1) a;
      List.iter (print_expr_node (i + 1)) es
    
    | EX_Tuple es ->
      pt i "EX_Tuple" typ span;
      List.iter (print_expr_node (i + 1)) es
    
    | EX_Construct (n, e) ->
      pt i ("EX_Construct " ^ Path.to_string n.item) typ span;
      if Option.is_some e
      then print_expr_node (i + 1) (Option.get e)
    
    | EX_Literal l ->
      pt i ("EX_Literal " ^ show_literal l) typ span
    
    | EX_Identifier {span=_; item=id; typ=_} ->
      pt i ("EX_Identifier " ^ Path.to_string id) typ span

    | EX_External n ->
      pt i ("EX_External " ^ n) typ span

    | EX_Error -> pt i "EX_Error" typ span

and print_toplevel_node i node =
  let {span; item} : toplevel node = node in
  match item with
    | TL_Declaration (n, t) ->
      pt i ("TL_Declaration " ^ n.item) t.item span;
    
    | TL_Definition { patt; expr } ->
      p i "TL_Definition" span;
      print_patt_node' (i + 1) patt;
      print_expr_node (i + 1) expr

    | TL_Section (n, tast) ->
      p i ("TL_Section " ^ n.item) span;
      print_tast (i + 1) tast

and print_tast i nodes =
  let f nl i (tl: toplevel node) =
    if Span.is_in_stdlib tl.span then ()
    else begin
      (if nl then print_newline ());
      print_toplevel_node i tl
    end
  in

  let rec go nl = function
    | [] -> ()
    | [n] -> f nl i n
    | n :: ns ->
      f nl i n;
      go (not (Span.is_in_stdlib n.span)) ns

  in go false nodes

(* expose functions *)

let print_expr_node = print_expr_node 0
let print_toplevel_node = print_toplevel_node 0
let print_tast = print_tast 0