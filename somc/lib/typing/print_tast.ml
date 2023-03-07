open Tast
open ANSITerminal

module Ident = Symbols.Ident

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
  | LIInt i -> "Int " ^ string_of_int i
  | LIFloat f -> "Float " ^ string_of_float f
  | LIChar c -> "Char '" ^ String.make 1 c ^ "'"
  | LIString s -> "String \"" ^ String.escaped s ^ "\""
  | LINil -> "Nil"

(* print functions *)

let rec print_patt_node' i node =
  let {span; item; typ} = node in
  match item with
    | PAVariable n ->
      pt i ("PAVariable " ^ n) typ span;
    | PAWildcard ->
      pt i "PAWildcard" typ span

and print_expr_node' i node =
  let {span; item; typ} = node in
  match item with
    | EXGrouping e -> 
      pt i "EXGrouping" typ span;
      print_expr_node' (i + 1) e
    
    | EXBinding (bind, e) ->
      pt i "EXBinding" typ span;
      print_patt_node' (i + 1) bind.vb_patt;
      print_expr_node' (i + 1) bind.vb_expr;
      print_expr_node' (i + 1) e
  
    | EXLambda {vb_patt; vb_expr} ->
      pt i "EXLambda" typ span;
      print_patt_node' (i + 1) vb_patt;
      print_expr_node' (i + 1) vb_expr
  
    | EXSequence (e1, e2) ->
      pt i "EXSequence" typ span;
      print_expr_node' (i + 1) e1;
      print_expr_node' (i + 1) e2
    
    | EXApplication (a, es) ->
      pt i "EXApplication" typ span;
      print_expr_node' (i + 1) a;
      List.iter (print_expr_node' (i + 1)) es
    
    | EXTuple es ->
      pt i "EXTuple" typ span;
      List.iter (print_expr_node' (i + 1)) es
    
    | EXConstruct (n, es) ->
      pt i ("EXConstruct " ^ Ident.to_string n.item) typ span;
      List.iter (print_expr_node' (i + 1)) es
    
    | EXLiteral l ->
      pt i ("EXLiteral " ^ show_literal l) typ span
    
    | EXIdentifier {span=_; item=id; typ=_} ->
      pt i ("EXIdentifier " ^ Ident.to_string id) typ span

    | EXMagical m ->
      pt i ("EXMagical " ^ Magicals.to_string m) typ span

    | EXError -> pt i "EXError" typ span

let print_toplevel_node' i node =
  let {span; item} : toplevel node = node in
  match item with
    | TLValueDef {vd_name; vd_expr} ->
      p i ("TLValueDef " ^ Ident.to_string vd_name.item) span;
      print_expr_node' (i + 1) vd_expr

    | TLTypeDef {td_name; td_type} ->
      p i ("TLTypeDef " ^ Ident.to_string td_name.item) span;
      pt (i + 1) "<type>" td_type.item td_type.span

    | TLExternDef {ed_native_name; ed_name; ed_type} ->
      p i ("TLExternDef " ^ Ident.to_string ed_name.item) span;
      pt (i + 1) ed_native_name.item ed_type.item ed_type.span

let print_tast' i nodes =
  let first = ref true in

  let f i (tl: toplevel node) =
    if Configs.hide_stdlib_nodes && Span.is_in_stdlib tl.span
    then ()
    else begin
      if !first then first := false
      else print_newline ();
      print_toplevel_node' i tl
    end
  in

  let rec go = function
    | [] -> ()
    | [n] -> f i n
    | n :: ns ->
      f i n;
      go ns
  in go nodes

(* expose functions *)

let print_expr_node = print_expr_node' 0

let print_toplevel_node = print_toplevel_node' 0

let print_tast = print_tast' 0