type t =
  | Prim_add_int
  | Prim_add_char
  | Prim_add_float
  | Prim_add_string
  | Prim_sub_int
  | Prim_sub_char
  | Prim_sub_float
  | Prim_mul_int
  | Prim_mul_char
  | Prim_mul_float
  | Prim_div_int
  | Prim_div_char
  | Prim_div_float
  | Prim_rem_int
  | Prim_rem_float
  | Prim_abs_int
  | Prim_abs_float
  | Prim_neg_int
  | Prim_neg_float
  | Prim_and
  | Prim_or
  | Prim_not
  | Prim_eq
  | Prim_eq_value
  | Prim_gt_int
  | Prim_gt_float
  | Prim_lt_int
  | Prim_lt_float
  | Prim_neq
  | Prim_neq_value
  | Prim_gteq_int
  | Prim_gteq_float
  | Prim_lteq_int
  | Prim_lteq_float
  | Prim_tageq

let names_assoc = [
  "add_int", Prim_add_int;
  "add_char", Prim_add_char;
  "add_float", Prim_add_float;
  "add_string", Prim_add_string;
  "sub_int", Prim_sub_int;
  "sub_char", Prim_sub_char;
  "sub_float", Prim_sub_float;
  "mul_int", Prim_mul_int;
  "mul_char", Prim_mul_char;
  "mul_float", Prim_mul_float;
  "div_int", Prim_div_int;
  "div_char", Prim_div_char;
  "div_float", Prim_div_float;
  "rem_int", Prim_rem_int;
  "rem_float", Prim_rem_float;
  "abs_int", Prim_abs_int;
  "abs_float", Prim_abs_float;
  "neg_int", Prim_neg_int;
  "neg_float", Prim_neg_float;
  "and", Prim_and;
  "or", Prim_or;
  "not", Prim_not;
  "eq", Prim_eq;
  "eq_value", Prim_eq_value;
  "gt_int", Prim_gt_int;
  "gt_float", Prim_gt_float;
  "lt_int", Prim_lt_int;
  "lt_float", Prim_lt_float;
  "neq", Prim_neq;
  "neq_value", Prim_neq_value;
  "gteq_int", Prim_gteq_int;
  "gteq_float", Prim_gteq_float;
  "lteq_int", Prim_lteq_int;
  "lteq_float", Prim_lteq_float;
  "tageq", Prim_tageq;
]

let arity = function
  | Prim_add_int | Prim_add_char | Prim_add_float | Prim_add_string
  | Prim_sub_int | Prim_sub_char | Prim_sub_float
  | Prim_mul_int | Prim_mul_char | Prim_mul_float
  | Prim_div_int | Prim_div_char | Prim_div_float
  | Prim_rem_int | Prim_rem_float
  | Prim_and | Prim_or
  | Prim_eq | Prim_eq_value
  | Prim_gt_int | Prim_gt_float
  | Prim_lt_int | Prim_lt_float
  | Prim_neq | Prim_neq_value
  | Prim_gteq_int | Prim_gteq_float
  | Prim_lteq_int | Prim_lteq_float
  | Prim_tageq -> 2

  | Prim_abs_int | Prim_abs_float
  | Prim_neg_int | Prim_neg_float
  | Prim_not -> 1

let find str = List.assoc str names_assoc

let to_string p =
  let ns, ps = List.split names_assoc in
  List.combine ps ns |> List.assoc p
  
