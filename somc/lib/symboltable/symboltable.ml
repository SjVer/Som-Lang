module Ident = Ident
module IMap = Map.Make(Ident)

open Ident

type 'a entry =
  {
    symbol: 'a;
    usages: Span.t list;
  }

(* NOTE: constructors can be stored as values
   as their names can never collide (uppercase) *)

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

let add_value_entry table ident entry =
  {table with values = IMap.add ident entry table.values}
let add_type_entry table ident entry =
  {table with types = IMap.add ident entry table.types}

let add_new_value table ident symbol =
  let entry = {symbol; usages = []} in
  {table with values = IMap.add ident entry table.values}

let add_new_type table ident symbol =
  let entry = {symbol; usages = []} in
  {table with types = IMap.add ident entry table.types}

(* might raise Not_found *)
let use_value table ident span =
  let entry = get_value table ident in
  let entry' = {entry with usages = entry.usages @ [span]} in
  {table with values = IMap.add ident entry' table.values}

(* might raise Not_found *)
let use_type table ident span =
  let entry = get_type table ident in
  let entry' = {entry with usages = entry.usages @ [span]} in
  {table with types = IMap.add ident entry' table.types}
  
(* will overwrite entries in [t1] with [t2] *)
let merge_tables t1 t2 =
  let values = IMap.fold IMap.add t1.values t2.values in
  let types = IMap.fold IMap.add t1.types t2.types in
  {values; types}

let check_submodule table ident =
  let keys map = List.map fst (IMap.bindings map) in
  let idents = keys table.values @ keys table.types in
  let f = function
    | Cons (_, Cons (hd, _)) -> hd = ident
    | _ -> false
  in
  List.exists f idents

let print_table table value_fn type_fn =
  print_endline "Values:";
  if IMap.is_empty table.values then
    print_endline "\t<none>"
  else
    IMap.iter begin fun i v ->
      Printf.printf "\t%s:\n" (to_string i);
      value_fn v;
    end table.values;
    
  print_endline "Types:";
  if IMap.is_empty table.types then
    print_endline "\t<none>"
  else
    IMap.iter begin fun i v ->
      Printf.printf "\t%s:\n" (to_string i);
      type_fn v;
    end table.types;
