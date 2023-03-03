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

let rec from_list = function
  | [] -> invalid_arg "from_list"
  | i :: [] -> Ident i
  | hd :: tl -> Cons (hd, from_list tl)

let rec to_list = function
  | Ident s -> [s]
  | Cons (hd, tl) -> hd :: to_list tl

let last i = List.hd (List.rev (to_list i))

let has_prefix ident prefix =
  let rec go i p =
    match i, p with
      | ihd :: itl, phd :: ptl when ihd = phd ->
        go itl ptl
      | _, _ :: _ -> false
      | _, [] -> true
  in
  go (to_list ident) (to_list prefix)

exception Empty_ident

let rec remove_prefix ident prefix =
  match ident, prefix with
    | Cons (ihd, itl), Cons (phd, ptl) when ihd = phd ->
      remove_prefix itl ptl
    | Cons (ihd, itl), Ident p when ihd = p ->
      itl
    | Ident i, Ident p when i = p -> raise Empty_ident
    | _ -> failwith "Ident.remove_prefix"