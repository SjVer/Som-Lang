open Ast

let p i s = print_endline (String.make i '\t' ^ s) 

let show_literal_node = function
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
  | Grouping e -> 
    p i "Grouping";
    print_expr_node' (i + 1) e
  | BinaryOp (o, e1, e2) ->
    p i ("BinaryOp " ^ show_bin_op o);
    print_expr_node' (i + 1) e1;
    print_expr_node' (i + 1) e2
  | UnaryOp (o, e) ->
    p i ("UnaryOp " ^ show_un_op o);
    print_expr_node' (i + 1) e
  | Literal l ->
    p i ("Literal " ^ show_literal_node l)

let print_expr_node = print_expr_node' 0