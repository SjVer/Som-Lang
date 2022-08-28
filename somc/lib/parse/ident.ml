type t =
  | Ident of string
  | Cons of t * string

let rec to_string = function
  | Ident s -> s
  | Cons (l, s) -> to_string l ^ "::" ^ s

let rec from_rev_list = function
  | [] -> failwith "from_rev_list"
  | [s] -> Ident s
  | s :: ss -> Cons (from_rev_list ss, s)

let from_list l = from_rev_list (List.rev l)