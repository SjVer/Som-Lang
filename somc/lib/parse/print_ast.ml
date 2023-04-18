open Ast
open ANSITerminal

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()

(** show functions *)

let show_literal = function
  | Pli_int i -> "Int " ^ string_of_int i
  | Pli_float f -> "Float " ^ string_of_float f
  | Pli_char c -> "Char '" ^ String.make 1 c ^ "'"
  | Pli_string s -> "String \"" ^ String.escaped s ^ "\""
  | Pli_null -> "Nil"

let rec show_path = function
  | [] -> ""
  | [p] -> p.item
  | p :: ps -> p.item ^ "::" ^ show_path ps

(* print functions *)

let rec print_patt_node' i { span; item } =
  match item with
    | Ppat_wildcard -> p i "Ppat_wildcard" span
    | Ppat_variable n -> p i ("Ppat_variable " ^ n) span
    | Ppat_literal l -> p i ("Ppat_literal " ^ show_literal l) span
    | Ppat_construct (c, args) ->
      p i ("Ppat_construct " ^ Ident.to_string c.item) span;
      List.iter (print_patt_node' (i + 1)) args 
    | Ppat_tuple patts ->
      p i "Ppat_tuple" span;
      List.iter (print_patt_node' (i + 1)) patts
    
and print_type_node' i { span; item } =
  match item with
    | Pty_grouping t ->
      p i "Pty_grouping" span;
      print_type_node' (i + 1) t

    | Pty_forall (args, t) ->
      let args' = List.map ((^) "'") (nmapi args) in 
      let args'' = String.concat " " args' in 
      p i ("Pty_forall " ^ args'') span;
      print_type_node' (i + 1) t

    | Pty_wildcard ->
      p i "Pty_wildcard" span

    | Pty_variable s ->
      p i ("Pty_variable '" ^ s.item) span

    | Pty_effect t ->
      p i "Pty_effect" span;
      print_type_node' (i + 1) t
    
    | Pty_function (a, r) ->
      p i "Pty_function" span;
      print_type_node' (i + 1) a;
      print_type_node' (i + 1) r
    
    | Pty_tuple ts ->
      p i "Pty_tuple" span;
      List.iter (print_type_node' (i + 1)) ts

    | Pty_construct (a, t) ->
      p i ("Pty_construct " ^ (Ident.to_string t.item)) span;
      if Option.is_some a
      then print_type_node' (i + 1) (Option.get a);

and print_complex_type_node' i { span; item } =
  match item with
    | Pct_variant rows ->
      p i "Pct_variant" span;
      let f (id, ts) = begin
        p (i + 1) (Ident.to_string id.item) id.span;
        List.iter (print_type_node' (i + 2)) ts
      end in List.iter f rows
    | Pct_simple t ->
      p i "Pct_simple" span;
      print_type_node' (i + 1) t

and print_case' i (patt, expr) =
  p i "<case>" (Span.concat_spans patt.span expr.span);
  print_patt_node' (i + 1) patt;
  print_expr_node' (i + 1) expr

and print_expr_node' i { span; item } =
  match item with
    | Pexp_grouping e -> 
      p i "Pexp_grouping" span;
      print_expr_node' (i + 1) e

    | Pexp_binding (bind, e) ->
      p i "Pexp_binding" span;
      print_patt_node' (i + 1) bind.vb_patt;
      print_expr_node' (i + 1) bind.vb_expr;
      print_expr_node' (i + 1) e

    | Pexp_lambda {vb_patt; vb_expr} ->
      p i "Pexp_lambda" span;
      print_patt_node' (i + 1) vb_patt;
      print_expr_node' (i + 1) vb_expr

    | Pexp_match (scrut, cases) ->
      p i "Pexp_match" span;
      print_expr_node' (i + 1) scrut;
      List.iter (print_case' (i + 1)) cases

    | Pexp_switch cases ->
      p i "Pexp_switch" span;
      List.iter (print_case' (i + 1)) cases

    | Pexp_sequence (e1, e2) ->
      p i "Pexp_sequence" span;
      print_expr_node' (i + 1) e1;
      print_expr_node' (i + 1) e2

    | Pexp_constraint (e, t) ->
      p i "EXContstraint" span;
      print_expr_node' (i + 1) e;
      print_type_node' (i + 1) t;

    | Pexp_apply (a, es) ->
      p i "Pexp_apply" span;
      print_expr_node' (i + 1) a;
      List.iter (print_expr_node' (i + 1)) es

    | Pexp_tuple es ->
      p i "Pexp_tuple" span;
      List.iter (print_expr_node' (i + 1)) es

    | Pexp_construct (n, es) ->
      p i ("Pexp_construct " ^ Ident.to_string n.item) span;
      List.iter (print_expr_node' (i + 1)) es 

    | Pexp_literal l ->
      p i ("Pexp_literal " ^ show_literal l) span
    
    | Pexp_ident {span = _; item = id} ->
      p i ("Pexp_ident " ^ Ident.to_string id) span
    
    | Pexp_magic n -> p i ("Pexp_magic " ^ n) span
    | Pexp_error -> p i "Pexp_error" span

and print_import_kind_node' i { span; item } =
  match item with
    | Pik_module -> p i "Pik_module" span
    | Pik_simple n -> p i ("Pik_simple " ^ show_path n) span
    | Pik_glob -> p i "Pik_glob" span
    | Pik_rename (s, d) ->
      p i ("Pik_rename " ^ show_path s ^ " as " ^ d.item) span
    | Pik_nested is ->
      p i "Pik_nested" span;
      List.iter (print_import_kind_node' (i + 2)) is

and print_toplevel_node' i { span; item } =
  match item with
    | Ptl_value_def {vd_name; vd_expr} ->
      p i ("Ptl_value_def " ^ Ident.to_string vd_name.item) span;
      print_expr_node' (i + 1) vd_expr

    | Ptl_type_def {td_params; td_name; td_type} ->
      let params =
        nmapi td_params
        |> List.map (fun p -> "'" ^ p ^ " ")
        |> String.concat ""
      in
      let name = params ^ Ident.to_string td_name.item in
      p i ("Ptl_type_def " ^ name) span;
      print_complex_type_node' (i + 1) td_type

    | Ptl_extern_def {ed_native_name; ed_name; ed_type} ->
      p i ("Ptl_extern_def " ^ Ident.to_string ed_name.item) span;
      p (i + 1) ed_native_name.item ed_native_name.span;
      print_type_node' (i + 1) ed_type;

    | Ptl_import { i_path; i_kind} ->
      p i ("Ptl_import " ^ show_path i_path) span;
      print_import_kind_node' (i + 1) i_kind

    | Ptl_module (n, ast) ->
      p i ("Ptl_module " ^ n.item) span;
      print_ast' (i + 1) ast
    
and print_ast' i nodes =
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
let print_type_node = print_type_node' 0
let print_toplevel_node = print_toplevel_node' 0
let print_ast = print_ast' 0