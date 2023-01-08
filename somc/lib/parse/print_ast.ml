open Ast
open ANSITerminal

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()

(** show functions *)

let show_literal = function
  | LI_Int i -> "Int " ^ string_of_int i
  | LI_Float f -> "Float " ^ string_of_float f
  | LI_Char c -> "Char '" ^ String.make 1 c ^ "'"
  | LI_String s -> "String \"" ^ String.escaped s ^ "\""
  | LI_Nil -> "Nil"

let show_primitive_type = function
  | PT_Int (Some (s, w)) ->
    Printf.sprintf "$i.%c.%d" (if s then 's' else 'u') w
  | PT_Float (Some w) -> Printf.sprintf "$f.%d" w
  | PT_Int None -> "$i.*"
  | PT_Float None -> "$f.*"
  | PT_Void -> "$v"

let rec show_path = function
  | [] -> ""
  | [p] -> p.item
  | p :: ps -> p.item ^ "::" ^ show_path ps

(* print functions *)

let rec print_patt_node' i node =
  let { span; item } = node in
  match item with
    | PA_Variable n ->
      p i ("PA_Variable " ^ n) span;
    
    | PA_Wildcard ->
      p i "PA_Wildcard" span
    
and print_type_node i node =
  let { span; item } = node in
  match item with
    | TY_Variant cs ->
      p i "TY_Variant" span;
      let f (s, ts) = begin
        p (i + 1) s.item s.span;
        List.iter (print_type_node (i + 2)) ts
      end in List.iter f cs

    | TY_Grouping t ->
      p i "TY_Grouping" span;
      print_type_node (i + 1) t

    | TY_Any ->
      p i "TY_Any" span

    | TY_Variable s ->
      p i ("TY_Variable '" ^s) span

    | TY_Effect t ->
      p i "TY_Effect" span;
      if Option.is_some t
      then print_type_node (i + 1) (Option.get t)
    
    | TY_Function (a, r) ->
      p i "TY_Function" span;
      print_type_node (i + 1) a;
      print_type_node (i + 1) r
    
    | TY_Tuple ts ->
      p i "TY_Tuple" span;
      List.iter (print_type_node (i + 1)) ts

    | TY_Construct (a, t) ->
      p i ("TY_Construct " ^ (Ident.to_string t.item)) span;
      if Option.is_some a
      then print_type_node (i + 1) (Option.get a);
    
    | TY_Primitive t ->
      p i ("TY_Primitive " ^ show_primitive_type t) span

and print_expr_node i node =
  let { span; item } = node in
  match item with
    | EX_Grouping e -> 
      p i "EX_Grouping" span;
      print_expr_node (i + 1) e

    | EX_Binding (bind, e) ->
      p i "EX_Binding" span;
      print_patt_node' (i + 1) bind.patt;
      print_expr_node (i + 1) bind.expr;
      print_expr_node (i + 1) e

    | EX_Lambda {patt; expr} ->
      p i "EX_Lambda" span;
      print_patt_node' (i + 1) patt;
      print_expr_node (i + 1) expr

    | EX_Sequence (e1, e2) ->
      p i "EX_Sequence" span;
      print_expr_node (i + 1) e1;
      print_expr_node (i + 1) e2

    | EX_Constraint (e, t) ->
      p i "EX_Contstraint" span;
      print_expr_node (i + 1) e;
      print_type_node (i + 1) t;

    | EX_Application (a, es) ->
      p i "EX_Application" span;
      print_expr_node (i + 1) a;
      List.iter (print_expr_node (i + 1)) es

    | EX_Tuple es ->
      p i "EX_Tuple" span;
      List.iter (print_expr_node (i + 1)) es

    | EX_Construct (n, es) ->
      p i ("EX_Construct " ^ Ident.to_string n.item) span;
      List.iter (print_expr_node (i + 1)) es 

    | EX_Literal l ->
      p i ("EX_Literal " ^ show_literal l) span
    
    | EX_Identifier {span = _; item = id} ->
      p i ("EX_Identifier " ^ Ident.to_string id) span
    
    | EX_External n ->
      p i ("EX_External " ^ n) span

    | EX_Error ->
      p i "EX_Error" span

and print_import_kind_node' i node =
  let {span; item} = node in
  match item with
    | IK_Glob -> p i "IK_Glob" span
    | IK_Simple n -> p i ("IK_Simple " ^ n.item) span;
    | IK_Self ik ->
      p i "IK_self" span;
      print_import_kind_node' (i + 1) ik
    | IK_Rename (s, d) ->
      p i ("IK_Rename " ^ s.item ^ " => " ^ d.item) span
    | IK_Nested is ->
      p i "IK_Nested" span;
      List.iter (
        fun {span; item = {dir=_; path; kind}} ->
          p (i + 1) (show_path path) span;
          print_import_kind_node' (i + 2) kind
        ) is
    | IK_Error -> p i "IK_error" span

and print_toplevel_node i node =
  let { span; item } = node in
  match item with
    | TL_Declaration (n, t) ->
      p i ("TL_Declaration " ^ n.item) span;
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

    | TL_Import {dir; path; kind} ->
      let dir' = match dir with
        | [] -> ""
        | l ->
          String.concat "/" (nmapi l) ^ "/"
      in
      p i ("TL_Import " ^ dir' ^ show_path path) span;
      print_import_kind_node' (i + 1) kind

    | TL_Section (n, ast) ->
      p i ("TL_Section " ^ n.item) span;
      print_ast (i + 1) ast
    
    | TL_Link (n, tl) ->
      p i ("TL_Link " ^ n) span;
      print_toplevel_node (i + 1) tl

and print_ast i nodes =
  let f nl i (tl: toplevel node) =
    if Config.hide_stdlib_nodes && Span.is_in_stdlib tl.span
    then ()
    else begin
      if nl then print_newline ();
      print_toplevel_node i tl
    end
  in

  let rec go nl = function
    | [] -> ()
    | [n] -> f nl i n
    | n :: ns ->
      f nl i n;
      go true ns
  in go false nodes

(* expose functions *)

let print_expr_node = print_expr_node 0
let print_type_node = print_type_node 0
let print_toplevel_node = print_toplevel_node 0
let print_ast = print_ast 0