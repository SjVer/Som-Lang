open Symbols.Ident
open Context

let bind_builtins ctx =
  let op str = from_list ["_std_ops"; str] in
  let next_id =
    let i = ref 0 in
    fun () -> decr i; !i
  in
  let value i q c =
    let entry = q, next_id () in
    let value_map = IMap.add (Ident i) entry c.value_map in
    {ctx with value_map}
  in

  ctx
  |> value "::" (from_list ["_std_list"; "Cons"])
  |> value "[]" (from_list ["_std_list"; "Nil"])
  |> value "^"  (op "pow")
  |> value "*"  (op "mul")
  |> value "/"  (op "div")
  |> value "%"  (op "mod")
  |> value "+"  (op "add")
  |> value "-"  (op "sub")
  |> value ">"  (op "gr")
  |> value ">=" (op "greq")
  |> value "<"  (op "ls")
  |> value "<=" (op "lseq")
  |> value "="  (op "eq")
  |> value "/=" (op "neq")
  |> value "&&" (op "and")
  |> value "^^" (op "xor")
  |> value "||" (op "or")
