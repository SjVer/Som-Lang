type t =
  | Ident of string
  | Cons of string * t

let rec to_string = function
  | Ident s -> s
  | Cons (l, s) -> l ^ "::" ^ to_string s

let compare a b = compare (to_string a) (to_string b)

let prepend hd tl = Cons (hd, tl)

let rec from_list = function
  | [] -> invalid_arg "from_list"
  | i :: [] -> Ident i
  | hd :: tl -> Cons (hd, from_list tl)

let rec to_list = function
  | Ident s -> [s]
  | Cons (hd, tl) -> hd :: to_list tl

let last i = List.hd (List.rev (to_list i))