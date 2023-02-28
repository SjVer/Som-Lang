module Ident = Ident
module IMap = Map.Make(Ident)
open Ident

let next_index =
  let counter = ref 0 in
  fun () -> incr counter; !counter

type 'a entry =
  {
    index: int;
    symbol: 'a;
    mutable uses: Span.t list;
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
  let entry = {index = next_index (); symbol; uses = []} in
  {table with values = IMap.add ident entry table.values}

let add_new_type table ident symbol =
  let entry = {index = next_index (); symbol; uses = []} in
  {table with types = IMap.add ident entry table.types}

(* might raise Not_found *)
let use_value table ident span =
  let entry = get_value table ident in
  entry.uses <- entry.uses @ [span]

(* might raise Not_found *)
let use_type table ident span =
  let entry = get_type table ident in
  entry.uses <- entry.uses @ [span]
  
(* will overwrite entries in [t1] with [t2] *)
let merge_tables t1 t2 =
  let values = IMap.fold IMap.add t1.values t2.values
  and types = IMap.fold IMap.add t1.types t2.types in
  {values; types}

let check_submodule table ident =
  let keys map = List.map fst (IMap.bindings map) in
  let idents = keys table.values @ keys table.types
  and f = function
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
      let u = List.length v.uses in
      Printf.printf "\t%s: (%d uses)\n" (to_string i) u;
      value_fn v;
    end table.values;
    
  print_endline "Types:";
  if IMap.is_empty table.types then
    print_endline "\t<none>"
  else
    IMap.iter begin fun i v ->
      let u = List.length v.uses in
      Printf.printf "\t%s: (%d uses)\n" (to_string i) u;
      type_fn v;
    end table.types;
