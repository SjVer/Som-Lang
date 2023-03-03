module Ident = Ident

type key = [`Val of Ident.t | `Type of Ident.t]

let usages_tbl: (key, Span.t list) Hashtbl.t =
  Hashtbl.create 10

let reset () = Hashtbl.reset usages_tbl

let get_usages ident =
  try Hashtbl.find usages_tbl ident
  with Not_found -> []

let use ident span =
  let usages' = get_usages ident @ [span] in
  Hashtbl.replace usages_tbl ident usages'