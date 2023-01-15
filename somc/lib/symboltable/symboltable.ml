module Ident = Ident

module IMap = Map.Make(Ident)

type 'a entry =
  {
    symbol: 'a;
    usages: Span.t list;
  }

type ('v, 't) t =
  {
    values: 'v entry IMap.t;
    types: 't entry IMap.t;
  }

type ('v, 't) symbol_table = ('v, 't) t

let empty = 
  {
    values = IMap.empty;
    types = IMap.empty;
  }

let get_value table ident = IMap.find ident table.values
let get_type table ident = IMap.find ident table.types

let add_new_value table ident symbol =
  let entry = {symbol; usages = []} in
  {table with values = IMap.add ident entry table.values}

let add_new_type table ident symbol =
  let entry = {symbol; usages = []} in
  {table with types = IMap.add ident entry table.types}

(* might raise Not_found *)
let use_value table ident span =
  let entry = IMap.find ident table.values in
  let entry' = {entry with usages = entry.usages @ [span]} in
  {table with values = IMap.add ident entry' table.values}

(* might raise Not_found *)
let use_type table ident span =
  let entry = IMap.find ident table.types in
  let entry' = {entry with usages = entry.usages @ [span]} in
  {table with types = IMap.add ident entry' table.types}
  
(* will overwrite entries in [t1] with [t2] *)
let merge_tables t1 t2 =
  let rec fold dest fn map =
    
  in
  let value_fn k v t = add_new_value t k v in
  let t1' = fold t1 value_fn t2.values in
  let type_fn k v t = add_new_type t k v in
  let t1'' = fold t1' type_fn t2.types in
  t1''

let print_table table value_fn type_fn =
  print_endline "Values:";
  if IMap.is_empty table.values then
    print_endline "\t<none>"
  else
    IMap.iter begin fun i v ->
      Printf.printf "\t%s:\n" (Ident.to_string i);
      value_fn v;
    end table.values;
    
  print_endline "Types:";
  if IMap.is_empty table.types then
    print_endline "\t<none>"
  else
    IMap.iter begin fun i v ->
      Printf.printf "\t%s:\n" (Ident.to_string i);
      type_fn v;
    end table.types;
