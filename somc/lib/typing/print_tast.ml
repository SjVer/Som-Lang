open Tast
open ANSITerminal

module Ident = Symbols.Ident

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()

let pt i str typ span =
  print_string [] (String.make i '\t' ^ str);
  let tstr = Types.show typ Configs.tast_print_debug in
  print_string [Foreground Cyan] (" : " ^ tstr);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()

(** show functions *)

let show_literal = function
  | Tli_int i -> "Int " ^ string_of_int i
  | Tli_float f -> "Float " ^ string_of_float f
  | Tli_char c -> "Char '" ^ String.make 1 c ^ "'"
  | Tli_string s -> "String \"" ^ String.escaped s ^ "\""
  | Tli_null -> "Nil"

(* print functions *)

let rec print_patt_node' i node =
  let {span; item; typ} = node in
  match item with
    | Tpat_wildcard -> pt i "Tpat_wildcard" typ span
    | Tpat_variable n -> pt i ("Tpat_variable " ^ n) typ span;
    | Tpat_literal l ->
      pt i ("Tpat_literal " ^ show_literal l) typ span;
    | Tpat_construct (c, args) ->
      pt i ("Ppat_construct " ^ Ident.to_string c.item) typ span;
      List.iter (print_patt_node' (i + 1)) args 
    | Tpat_tuple patts ->
      pt i "Tpat_tuple" typ span;
      List.iter (print_patt_node' (i + 1)) patts

let rec print_case' i (patt, expr) =
  p i "<case>" (Span.concat_spans patt.span expr.span);
  print_patt_node' (i + 1) patt;
  print_expr_node' (i + 1) expr

and print_expr_node' i node =
  let {span; item; typ} = node in
  match item with
    | Texp_grouping e -> 
      pt i "Texp_grouping" typ span;
      print_expr_node' (i + 1) e
    
    | Texp_binding (bind, e) ->
      pt i "Texp_binding" typ span;
      print_patt_node' (i + 1) bind.vb_patt;
      print_expr_node' (i + 1) bind.vb_expr;
      print_expr_node' (i + 1) e
  
    | Texp_lambda {vb_patt; vb_expr} ->
      pt i "Texp_lambda" typ span;
      print_patt_node' (i + 1) vb_patt;
      print_expr_node' (i + 1) vb_expr
  
    | Texp_match (scrut, cases) ->
      p i "Texp_match" span;
      print_expr_node' (i + 1) scrut;
      List.iter (print_case' (i + 1)) cases

    | Texp_switch cases ->
      p i "Texp_switch" span;
      List.iter (print_case' (i + 1)) cases
    
    | Texp_if (cond, texp, eexp) ->
      p i "Texp_if" span;
      print_expr_node' (i + 1) cond;
      print_expr_node' (i + 1) texp;
      print_expr_node' (i + 1) eexp
   
    | Texp_sequence (e1, e2) ->
      pt i "Texp_sequence" typ span;
      print_expr_node' (i + 1) e1;
      print_expr_node' (i + 1) e2
    
    | Texp_apply (a, es) ->
      pt i "Texp_apply" typ span;
      print_expr_node' (i + 1) a;
      List.iter (print_expr_node' (i + 1)) es
    
    | Texp_tuple es ->
      pt i "Texp_tuple" typ span;
      List.iter (print_expr_node' (i + 1)) es
    
    | Texp_construct (n, es) ->
      pt i ("Texp_construct " ^ Ident.to_string n.item) typ span;
      List.iter (print_expr_node' (i + 1)) es
    
    | Texp_literal l ->
      pt i ("Texp_literal " ^ show_literal l) typ span
    
    | Texp_ident {span=_; item=id; typ=_} ->
      pt i ("Texp_ident " ^ Ident.to_string id) typ span

    | Texp_primitive m ->
      pt i ("Texp_primitive " ^ Symbols.Primitive.to_string m) typ span

    | Texp_error -> pt i "Texp_error" typ span

let print_toplevel_node' i node =
  let {span; item} : toplevel node = node in
  match item with
    | Ttl_value_def {vd_name; vd_expr} ->
      p i ("Ttl_value_def " ^ Ident.to_string vd_name.item) span;
      print_expr_node' (i + 1) vd_expr

    | Ttl_type_def {td_name; td_type} ->
      p i ("Ttl_type_def " ^ Ident.to_string td_name.item) span;
      pt (i + 1) "<type>" td_type.item td_type.span

    | Ttl_extern_def {ed_native_name; ed_name; ed_type} ->
      p i ("Ttl_extern_def " ^ Ident.to_string ed_name.item) span;
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
