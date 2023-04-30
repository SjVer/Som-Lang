type t =
  | Magic_add
  | Magic_sub
  | Magic_mul
  | Magic_div
  | Magic_rem
  | Magic_abs
  | Magic_neg
  | Magic_and
  | Magic_or
  | Magic_not
  | Magic_eq
  | Magic_gt
  | Magic_lt
  | Magic_neq
  | Magic_gteq
  | Magic_lteq
  | Magic_tageq

let names_assoc = [
  ("add",  Magic_add);
  ("sub",  Magic_sub);
  ("mul",  Magic_mul);
  ("div",  Magic_div);
  ("rem",  Magic_rem);
  ("abs",  Magic_abs);
  ("neg",  Magic_neg);
  ("and",  Magic_and);
  ("or",   Magic_or);
  ("not",  Magic_not);
  ("eq",   Magic_eq);
  ("gt",   Magic_gt);
  ("lt",   Magic_lt);
  ("neq",  Magic_neq);
  ("gteq", Magic_gteq);
  ("lteq", Magic_lteq);
  ("tageq",  Magic_tageq);
]

let arity = function
  | Magic_add | Magic_sub
  | Magic_mul | Magic_div
  | Magic_rem
  | Magic_and | Magic_or
  | Magic_eq  | Magic_neq
  | Magic_gt  | Magic_gteq
  | Magic_lt  | Magic_lteq
  | Magic_tageq -> 2
  
  | Magic_abs | Magic_neg
  | Magic_not -> 1
  

let find str = List.assoc str names_assoc

let to_string m =
  let ns, ms = List.split names_assoc in
  List.combine ms ns |> List.assoc m
  
