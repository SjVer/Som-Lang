type 'a node = {span: Span.span; item: 'a}

type literal_node =
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | String of string
  
type bin_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Power

type un_op =
  | Negate
  | Not

type expr_node =
  | Grouping of expr_node
  | BinaryOp of bin_op * expr_node * expr_node
  | UnaryOp of un_op * expr_node
  | Literal of literal_node
