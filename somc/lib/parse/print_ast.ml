open Ast
open ANSITerminal

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()

(** show functions *)

let show_literal = function
  | LI_Bool b -> "Bool " ^ string_of_bool b
  | LI_Int i -> "Int " ^ string_of_int i
  | LI_Float f -> "Float " ^ string_of_float f
  | LI_Char c -> "Char '" ^ String.make 1 c ^ "'"
  | LI_String s -> "String \"" ^ String.escaped s ^ "\""
  | LI_Nil -> "Nil"

let show_bin_op = function
  | BI_Add -> "+"
  | BI_Subtract -> "-"
  | BI_Multiply -> "*"
  | BI_Divide -> "/"
  | BI_Power -> "^"

let show_un_op = function
  | UN_Negate -> "-"
  | UN_Not -> "!"

let show_builtin_type = function
  | BT_Int (s, w) ->
    Printf.sprintf "$i.%c.%d"
    (if s then 's' else 'u') w
  | BT_Float w -> Printf.sprintf "$f.%d" w
  | BT_Void -> "$v"

let rec show_path = function
  | [] -> ""
  | [p] -> p.item
  | p :: ps -> p.item ^ "::" ^ show_path ps

(* print functions *)

let rec print_expr_node' i node =
  let { span; item } = node in match item with
  | EX_Grouping e -> 
    p i "EX_Grouping" span;
    print_expr_node' (i + 1) e

  | EX_Binding (bindings, e) ->
    p i "EX_Binding" span;
    List.iter begin fun { patt; expr } ->
      print_patt_node' (i + 1) patt;
      print_expr_node' (i + 2) expr
    end bindings;
    print_expr_node' (i + 1) e

  | EX_Lambda {patt; expr} ->
    p i "EX_Lambda" span;
    print_patt_node' (i + 1) patt;
    print_expr_node' (i + 1) expr

  | EX_Sequence (e1, e2) ->
    p i "EX_Sequence" span;
    print_expr_node' (i + 1) e1;
    print_expr_node' (i + 1) e2

  | EX_Application (a, es) ->
    p i "EX_Application" span;
    print_appl_node' (i + 1) a;
    List.iter (print_expr_node' (i + 1)) es

  (* | EX_Cast (e, t) ->
    p i "EX_Cast" span;
    print_expr_node' (i + 1) e;
    print_type_node' (i + 1) t *)

  | EX_Literal l ->
    p i ("EX_Literal " ^ show_literal l) span
  
  | EX_Ident v ->
    p i ("EX_Ident " ^ v) span

and print_appl_node' i node =
  let { span; item } = node in match item with
  | AP_Expr e -> p i "AP_Expr" span; print_expr_node' (i + 1) e
  | AP_BinaryOp o -> p i ("AP_BinaryOp " ^ show_bin_op o) span
  | AP_UnaryOp o -> p i ("AP_UnaryOp " ^ show_un_op o) span

and print_patt_node' i node =
  let { span; item } = node in match item with
  | PA_Variable n ->
    p i ("PA_Variable " ^ n) span;
  
  | PA_Wildcard ->
    p i "PA_Wildcard" span

and print_type_node' i node =
  let { span; item } = node in match item with
  | TY_Grouping t ->
    p i "TY_Grouping" span;
    print_type_node' (i + 1) t

  | TY_Any ->
    p i "TY_Any" span

  | TY_Var s ->
    p i ("TY_Var '" ^s) span

  | TY_Effect t ->
    p i "TY_Effect" span;
    if Option.is_some t
    then print_type_node' (i + 1) (Option.get t)
  
  | TY_Function (ts, r) ->
    p i "TY_Function" span;
    List.iter (print_type_node' (i + 1)) ts;
    print_type_node' (i + 1) r
  
  | TY_Tuple ts ->
    p i "TY_Tuple" span;
    List.iter (print_type_node' (i + 1)) ts

  | TY_List t ->
    p i "TY_List" span;
    print_type_node' (i + 1) t

  | TY_Constr (a, t) ->
    p i ("TY_Constr " ^ t) span;
    if Option.is_some a
    then print_type_node' (i + 1) (Option.get a);
  
  | TY_Alias (t, n) ->
    p i ("TY_Alias '" ^ n.item) span;
    print_type_node' (i + 1) t

  | TY_Builtin t ->
    p i ("TY_Builtin " ^ show_builtin_type t) span

and print_import_kind_node' i node =
  let {span; item} = node in match item with
  | IK_Simple -> p i "IK_Simple" span
  | IK_Glob -> p i "IK_Glob" span
  | IK_Rename n -> p i ("IK_Rename " ^ n) span
  | IK_Nested is -> p i "IK_Nested" span;
    List.iter (
      fun {span; item = {path; kind}} ->
        p (i + 1) (show_path path) span;
        print_import_kind_node' (i + 2) kind
      ) is

and print_toplevel_node' i node =
  let { span; item } = node in match item with
  | TL_Declaration (n, t) ->
    p i ("TL_Declaration " ^ n) span;
    print_type_node' (i + 1) t
  
  | TL_Definition { patt; expr } ->
    p i "TL_Definition" span;
    print_patt_node' (i + 1) patt;
    print_expr_node' (i + 1) expr

  | TL_Import {path; kind} ->
    p i ("TL_Import " ^ show_path path) span;
    print_import_kind_node' (i + 1) kind

(* helper functions *)

let print_expr_node = print_expr_node' 0
let print_type_node = print_type_node' 0
let print_toplevel_node = print_toplevel_node' 0
let rec print_toplevel nodes =
  match nodes with
  | [] -> ()
  | [n] -> print_toplevel_node n
  | n :: ns ->
    print_toplevel_node n;
    print_newline ();
    print_toplevel ns 