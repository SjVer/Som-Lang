open Parse.Ast
open Report.Error
open Symboltable
open Ident
open Canonicalize

(* 
  algorithm pseudocode:
  
  let resolve ast =
      symbol_table table;
      (import, symbol_table) imports;

      for tl in ast:
        if tl is import statement:
          imports += tl.import, resolve imported_ast
        else:
          add_to_table_by_absolute_path tl
      
      for (im, im_table) in imports:
        apply_import im im_table to table
*)

type ast_symbol_table = (value_definition, type_definition) symbol_table

let get_ast_symbol_table:
  (string -> Span.t -> ast_symbol_table) ref =
  ref (fun _ -> assert false)

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
  
(* error helper functions *)

let file_not_found_error path =
  let e = Failed_to_import (path.item ^ Config.extension) in
  Report.make_error (Other_error e) (Some path.span)
  |> Report.add_note (Printf.sprintf
    "try adding directory '%s%s' or\n\
    file '%s%s' to the search paths."
    path.item Filename.dir_sep path.item Config.extension)
  |> Report.raise

let cannot_import_from_dir_error dir span =
  let err = Cannot_import_dir (dir ^ Filename.dir_sep) in
  let report = Report.make_error (Other_error err) (Some span) in
  let note =
    if not (Span.is_in_stdlib {span with file = dir}) then
      Printf.sprintf
        "try adding file '%s%s'\n\
        and exposing any sub-modules there."
        dir Config.extension
    else
      "This directory is part of the standard library.\n\
      Perhaps the import statement is mispelled?"
  in
  Report.add_note note report |> Report.raise

let mod_has_no_symbol_error mname what sname =
  let e = Has_no_symbol (mname, what, sname.item) in
  Report.make_error (Type_error e) (Some sname.span)
  |> Report.raise

let shadowed_symbol_warning n s =
  let msg = Printf.sprintf "import shadows previous import of `%s`" n in
  Report.make_warning msg (Some s)
  |> Report.report

let check_import_private n =
  if n.item.[0] = '_' then
    let e = Cannot_private ("import", n.item) in
    Report.make_error (Type_error e) (Some n.span)
    |> Report.report

(* file stuff *)

let check_full_path_type full_path =
  if Sys.(file_exists full_path && is_directory full_path) then `Dir
  else if Sys.file_exists (full_path ^ Config.extension) then `File
  else raise Not_found

let rec recurse_in_dir dir = function
  | hd :: tl -> begin
      let full_path = Filename.concat dir hd.item in
      match check_full_path_type full_path with 
        | `Dir -> recurse_in_dir full_path tl
        | `File -> `File, full_path ^ Config.extension, tl
    end
  | [] -> `Dir, dir, []

let find_dir_or_file path =
  let search_dirs =
    "" (* CWD *)
    :: !(Config.Cli.args).search_dirs
    @ [Config.include_dir]
  in
  let rec go = function
    | hd :: tl -> begin
        try recurse_in_dir hd path
        with Not_found -> go tl
      end
    | [] -> file_not_found_error (List.hd path) 
  in
  go search_dirs

let find_file full_path span =
  let full_path' = [{item = full_path; span}] in
  match find_dir_or_file full_path' with
    | `File, file_path, [] -> file_path
    | _ -> full_path

(* main functionality *)

let get_symbols_in_ast ast =
  let go t tl = match tl.item with
    | TL_Value_Definition vdef ->
      let ident = Ident.Ident vdef.vd_name.item in
      add_new_value t ident vdef
    | TL_Type_Definition tdef ->
      let ident = Ident.Ident tdef.td_name.item in
      add_new_type t ident tdef
    | TL_Module (n, ast) ->
      let mod_table = canon_ast n.item ast in
      merge_tables t mod_table
    | _ -> t
  in
  List.fold_left go Symboltable.empty ast

let decide_on_file_and_path imp =
  let kind, full_path, path' = find_dir_or_file imp.i_path in

  (* if we found a directory, we must be importing a file *)
  if kind = `Dir then begin
    let dir = full_path in
    let ident = match imp.i_kind.item with
      | IK_Simple ident -> ident
      | _ ->
        let span = (List.hd (List.rev imp.i_path)).span in
        cannot_import_from_dir_error dir span
    in
    let file_basename = Filename.concat dir ident.item in
    let file_path =
      (* if there was no dir we haven't checked
      in which search dir the imported file belongs *)
      if dir = "" then find_file file_basename ident.span
      else file_basename ^ Config.extension
    in
    file_path, path', true
  end else
    full_path, path', false

let rec extract_submodule table mod_name = function
  | hd :: tl ->
    if not (check_submodule table hd.item) then
      let e = Type_error (Has_no_symbol (mod_name, "submodule", hd.item)) in
      Report.make_error e (Some hd.span)
      |> Report.raise
    else
      let table' = extract_prefixed table hd.item in
      extract_submodule table' hd.item tl
  | [] -> table, mod_name
    
let find_and_add_value_or_type src old_ident dest new_ident =
  (* TODO: allow importing both *)
  try add_value_entry dest new_ident (get_value src old_ident)
  with Not_found ->
    add_type_entry dest new_ident (get_type src old_ident)

let rec apply_import _mod_name table (imp, span) =
  (* find file and remainder of path *)
  let file, path, importing_file = decide_on_file_and_path imp in
  let imp_mod_name = Filename.(chop_extension (basename file)) in
  
  (* parse and get symbols from the file *)
  let raw_imp_table = !get_ast_symbol_table file span in 
  let imp_table = canon_table imp_mod_name raw_imp_table in

  (* merge tables to preserve all symbols *)
  let table' = merge_tables table imp_table in
  
  (* allow nested imports *)
  let rec finish destt srct mod_name path kind =
    (* apply the path *)
    let res_table, mod_name' = extract_submodule srct mod_name path in

    (* apply the import kind *)
    match kind.item with
      | IK_Simple n -> begin
          let old_ident = from_list (nmapi path @ [n.item]) in
          let new_ident = Ident n.item in
          try find_and_add_value_or_type res_table old_ident destt new_ident
          with Not_found -> mod_has_no_symbol_error mod_name' "symbol" n
        end
      | IK_Glob ->
        let res_table' = extract_prefixed res_table mod_name in
        merge_tables destt res_table'
      | IK_Rename (on, nn) -> begin
          let old_ident = from_list (nmapi path @ [on.item]) in
          let new_ident = Ident nn.item in
          try find_and_add_value_or_type res_table old_ident destt new_ident
          with Not_found -> mod_has_no_symbol_error mod_name' "symbol" on
        end
      | IK_Nested imps ->
        let f srct' imp = finish destt srct' mod_name' imp.i_path imp.i_kind in
        List.fold_left f res_table (nmapi imps)
  in

  (* if we're importing a file the IK_Simple is already handled *)
  if importing_file then
    table' (* the tables are already merged *)
  else
    let new_table = finish table' imp_table imp_mod_name path imp.i_kind in
    merge_tables table new_table

and resolve mod_name (ast : ast) : ast_symbol_table =
  let table = get_symbols_in_ast ast in
  let imports =
    let rec go acc = function
      | {item = TL_Import i; span} :: tls -> go (acc @ [i, span]) tls
      | _ :: tls -> go acc tls
      | [] -> acc
    in
    go [] ast
  in
  let try_apply_import t i =
    try apply_import mod_name t i
    with Report.Error e -> Report.report e; t
  in
  List.fold_left try_apply_import table imports