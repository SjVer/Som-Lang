open Ast
open ANSITerminal

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span span);
  print_newline ()

(** temp. function *)
let show_pattern = function
  | { span = _; item } -> match item with
    | P_Variable name -> "P_Variable " ^ name
    | P_Wildcard -> "P_Wildcard"

let show_literal = function
  | L_Bool b -> "Bool " ^ string_of_bool b
  | L_Int i -> "Int " ^ string_of_int i
  | L_Float f -> "Float " ^ string_of_float f
  | L_Char c -> "Char '" ^ String.make 1 c ^ "'"
  | L_String s -> "String \"" ^ String.escaped s ^ "\""

let show_bin_op = function
  | B_Add -> "+"
  | B_Subtract -> "-"
  | B_Multiply -> "*"
  | B_Divide -> "/"
  | B_Power -> "^"

let show_un_op = function
  | U_Negate -> "-"
  | U_Not -> "!"

let rec print_expr_node' i = function
  | { span; item } -> match item with

    | E_Grouping e -> 
      p i "E_Grouping" span;
      print_expr_node' (i + 1) e

    | E_Binding (bindings, e) ->
      p i "E_Binding" span;
      List.iter begin fun { patt; expr } ->
        p (i + 1) (show_pattern patt) expr.span;
        print_expr_node' (i + 2) expr
      end bindings;
      print_expr_node' (i + 1) e

    | E_Sequence (e1, e2) ->
      p i "E_Sequence" span;
      print_expr_node' (i + 1) e1;
      print_expr_node' (i + 1) e2

    | E_Application (a, es) ->
      p i "E_Application" span;
      print_appl_node (i + 1) a;
      List.iter (print_expr_node' (i + 1)) es

    | E_Literal l ->
      p i ("E_Literal " ^ show_literal l) span
    
    | E_Ident v ->
      p i ("E_Ident " ^ v) span

and print_appl_node i node =
  let { span; item } = node in match item with
  | A_Expr e -> p i "A_Expr" span; print_expr_node' (i + 1) e
  | A_BinaryOp o -> p i ("A_BinaryOp " ^ show_bin_op o) span
  | A_UnaryOp o -> p i ("A_UnaryOp " ^ show_un_op o) span

let print_expr_node = print_expr_node' 0