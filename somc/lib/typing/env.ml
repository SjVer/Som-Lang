open Parse.Ast
module IMap = Map.Make(Ident)

(* externals *)

let externals: (string, Types.t) Hashtbl.t = Hashtbl.create 10 

(* env stuff *)

type t =
  {
    values: Types.t IMap.t;
    aliases: Types.t IMap.t;
  }

let print env =
  print_endline "==============================";

  let f i t =
    Printf.printf "\t%s -> %s\n"
      (Ident.to_string i)
      (Types.show t true)
  in

  print_endline "Values: ";
  if IMap.is_empty env.values then print_endline "\t<none>"
  else IMap.iter f env.values;
  print_newline ();

  print_endline "Aliases: ";
  if IMap.is_empty env.aliases then print_endline "\t<none>"
  else IMap.iter f env.aliases;
  print_newline ();

  print_endline "=============================="

let empty =
  {
    values = IMap.empty;
    aliases = IMap.empty;
  }

(* finding *)

let lookup_value env ident =
  try IMap.find ident env.values
  with Not_found ->
    let n = Ident.to_string ident in
    failwith ("Value " ^ n ^ " not in env")

let lookup_alias env ident =
  try IMap.find ident env.aliases
  with Not_found ->
    let n = Ident.to_string ident in
    failwith ("Alias " ^ n ^ " not in env")

(* adding *)

let add_value env ident typ =
  {env with values = IMap.add ident typ env.values}

let add_alias env ident typ =
  {env with aliases = IMap.add ident typ env.aliases}
