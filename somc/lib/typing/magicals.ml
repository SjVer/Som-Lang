type t =
  | MAdd
  | MSub
  | MMul
  | MDiv
  | MRem
  | MAbs
  | MNeg
  | MAnd
  | MOr
  | MNot
  | MEq
  | MGt
  | MLt
  | MNeq
  | MGteq
  | MLteq

let names_assoc = [
  ("add",  MAdd);
  ("sub",  MSub);
  ("mul",  MMul);
  ("div",  MDiv);
  ("rem",  MRem);
  ("abs",  MAbs);
  ("neg",  MNeg);
  ("and",  MAnd);
  ("or",   MOr);
  ("not",  MNot);
  ("eq",   MEq);
  ("gt",   MGt);
  ("lt",   MLt);
  ("neq",  MNeq);
  ("gteq", MGteq);
  ("lteq", MLteq);
]

let find str = List.assoc str names_assoc

let to_string m =
  let ns, ms = List.split names_assoc in
  List.combine ms ns |> List.assoc m

let type_of =
  let open Types in
  (* TODO: expand on this *)
  let int = TVague (ref VGInt) in
  (* let int = TPrim (PInt (true, 32)) in *)
  let int_fn = TFun (int, int) in
  let ints_fn = TFun (int, int_fn) in
  function
    | MAdd
    | MSub 
    | MMul 
    | MDiv  -> ints_fn 
    | MRem  -> ints_fn
    | MAbs
    | MNeg  -> int_fn 
    | MAnd
    | MOr   -> ints_fn
    | MNot  -> int_fn
    | MEq
    | MGt 
    | MLt 
    | MNeq 
    | MGteq
    | MLteq -> ints_fn 