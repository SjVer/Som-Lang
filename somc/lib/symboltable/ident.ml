type t =
  | Ident of string
  | Cons of string * t

let rec to_string = function
  | Ident s -> s
  | Cons (l, s) -> l ^ "::" ^ to_string s

let compare a b = compare (to_string a) (to_string b)

let rec prepend prefix ident =
  match prefix with
    | Ident s -> Cons (s, ident)
    | Cons (s, l) -> Cons (s, prepend l ident)

let rec append ident suffix =
  match ident with
    | Ident s -> Cons (s, suffix)
    | Cons (s, l) -> Cons (s, append l suffix) 

let prepend_opt prefix ident =
  match ident with
    | Some ident -> prepend prefix ident
    | None -> prefix

let append_opt ident suffix =
  match ident with
    | Some ident -> append ident suffix
    | None -> suffix

let rec from_list = function
  | [] -> invalid_arg "from_list"
  | i :: [] -> Ident i
  | hd :: tl -> Cons (hd, from_list tl)

let rec to_list = function
  | Ident s -> [s]
  | Cons (hd, tl) -> hd :: to_list tl

let last i = List.hd (List.rev (to_list i))