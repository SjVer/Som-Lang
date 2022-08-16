open Ast
open ANSITerminal

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span span);
  print_newline ()

let show_literal = function
  | Bool b -> "Bool " ^ string_of_bool b
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Char c -> "Char '" ^ String.make 1 c ^ "'"
  | String s -> "String \"" ^ String.escaped s ^ "\""

let show_bin_op = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Power -> "^"

let show_un_op = function
  | Negate -> "-"
  | Not -> "!"

let rec print_expr_node' i = function
  | { span; item } -> match item with
    | Grouping e -> 
      p i "Grouping" span;
      print_expr_node' (i + 1) e
    | BinaryOp (o, e1, e2) ->
      p i ("BinaryOp " ^ show_bin_op o) span;
      print_expr_node' (i + 1) e1;
      print_expr_node' (i + 1) e2
    | UnaryOp (o, e) ->
      p i ("UnaryOp " ^ show_un_op o) span;
      print_expr_node' (i + 1) e
    | Literal l ->
      p i ("Literal " ^ show_literal l) span

let print_expr_node = print_expr_node' 0