(* 
  an extractor is a series of functions, that
  when applied to a value "extract" the identifier
  from it. so for the pattern
    `switch (_, _, Some x) -> x`
  identifier x would have the extractor
    `[Extr_get 2; Extr_tag <tag>; Extr_get 0]`
*)

type extract =
  | Extr_get of int (* n'th element of tuple-like *)
  | Extr_tag of int (* elements of value with tag *)

type extractor = extract list

