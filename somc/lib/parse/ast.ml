type 'a node = {span: Span.span; item: 'a}

type literal =
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

type expr =
  | Grouping of expr node
  | BinaryOp of bin_op * expr node * expr node
  | UnaryOp of un_op * expr node
  | Literal of literal
