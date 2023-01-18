open Parse.Ast
open Symboltable
open Ident

module St = Symboltable
module IMap = Map.Make(Ident)

(* symbol table stuff *)

type ast_symbol_table = (value_definition, type_definition) symbol_table

let print_ast_table (table : ast_symbol_table) =
  let open Symboltable in
  let valuefn {symbol = bind; usages = _} =
    Parse.PrintAst.p 2
      ("<def value " ^ bind.vd_name.item ^ ">")
      bind.vd_name.span;
      Parse.PrintAst.print_expr_node' 3 bind.vd_expr;
      print_newline ()
  in
  let typefn {symbol = bind; usages = _} =
    let rec join = function
      | [] -> ""
      | v :: vs -> "'" ^ v.item ^ " " ^ join vs
    in let name = join bind.td_params ^ bind.td_name.item in
    Parse.PrintAst.p 2
      ("<def type " ^ name ^ ">")
      bind.td_name.span;
    Parse.PrintAst.print_type_node' 3 bind.td_type;
    print_newline ()
  in
  print_table table valuefn typefn

(* scope stuff *)

(* TODO: seperate t.map for types and values? *)
(* TODO: add map for modules? *)

(* maps unresolved identifiers to resolved ones *)
type t =
  {
    name: Ident.t option;
    map: Ident.t IMap.t;
    table: ast_symbol_table;
  }

type scope = t

let empty name =
  {
    name = Some name;
    map = IMap.empty;
    table = Symboltable.empty;
  }

let nameless_empty =
  {
    name = None;
    map = IMap.empty;
    table = Symboltable.empty;
  }

let push s name =
  {s with name = Some (append_opt s.name (Ident name))}

let print s =
  let n' = match s.name with
    | Some n -> to_string n
    | None -> "============"
  in
  print_endline ("======== " ^ n' ^ " ========");
  print_endline "Bindings: ";
  let f k v = Printf.printf "    %s -> %s\n"
    (Ident.to_string k)
    (Ident.to_string v)
  in
  IMap.iter f s.map;
  print_ast_table s.table;
  print_endline "=============================="

let lookup_or_error s exists_fn ident what span =
  try IMap.find ident s.map
  with Not_found ->
    if exists_fn s ident then ident
    else
      let open Report.Error in
      let ident' = Ident.to_string ident in
      let e = Type_error (Use_of_unbound (what, ident')) in
      Report.report (Report.make_error e (Some span));
      (* TODO: error is not fatal now? *)
      Stdlib.raise Not_found

let lookup_value_or_error s ident span =
  let f s i = IMap.exists (fun k _ -> k = i) s.table.values in
  let first_chr = String.get (last ident) 0 in
  let what =
    if Char.uppercase_ascii first_chr = first_chr
      then "constructor" else "value"
  in
  lookup_or_error s f ident what span

let lookup_type_or_error s ident span =
  let f s i = IMap.exists (fun k _ -> k = i) s.table.types in
  lookup_or_error s f ident "type" span

let maybe_map_ident s ogid resid =
  let map =
    if ogid <> resid then IMap.add ogid resid s.map
    else s.map
  in
  {s with map}

let add_new_value s ogid resid sym =
  let table = St.add_new_value s.table resid sym in
  maybe_map_ident {s with table} ogid resid
  
let add_new_type s ogid resid sym =
  let table = St.add_new_type s.table resid sym in
  maybe_map_ident {s with table} ogid resid

let add_table s t = {s with table = merge_tables s.table t}

let merge_scopes s1 s2 =
  {
    s1 with
    map = IMap.fold IMap.add s1.map s2.map;
    table = merge_tables s1.table s2.table;
  }

let extract_prefixed s prefix =
  let filter k _ = match k with
    | Cons (_, Cons (hd, _)) when hd = prefix -> true
    | _ -> false
  and fold k e m = match k with
    | Cons (_, Cons (hd, tl)) when hd = prefix -> IMap.add tl e m
    | _ -> assert false
  in
  let f m = IMap.fold fold (IMap.filter filter m) IMap.empty in

  {
    name = Some (Ident prefix);
    table =
      {
        values = f s.table.values;
        types = f s.table.types;
      };
    map = f s.map;
  }