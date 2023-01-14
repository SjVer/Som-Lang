type t =
  | Ident of string
  | Cons of t * string

let rec to_string = function
  | Ident s -> s
  | Cons (l, s) -> to_string l ^ "::" ^ s

let compare a b = compare (to_string a) (to_string b)

let rec from_rev_list = function
  | [] -> invalid_arg "from_rev_list"
  | [s] -> Ident s
  | s :: ss -> Cons (from_rev_list ss, s)

let from_list = function
  | [] -> invalid_arg "from_list"
  | l -> from_rev_list (List.rev l)

let rec to_list = function
  | Ident s -> [s]
  | Cons (p, s) -> to_list p @ [s]

let last p =
  let Ident s | Cons (_, s) = p 
  in s