open Parse.Ast
open Report.Error
open Symboltable
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

let get_ast_fn:
  (string -> Span.t -> ast) ref =
  ref (fun _ _ : ast -> assert false)

let print_ast_table (table : ast_symbol_table) =
  let open Symboltable in
  let valuefn {symbol = bind; usages = _} =
    Parse.PrintAst.p 2
      ("TL_Value_Definition " ^ bind.vd_name.item)
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
      ("TL_Type_Definition " ^ name)
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
    Filename.dir_sep path.item path.item Config.extension)
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
    | [] -> raise Not_found
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
      let ident = Ident.Ident n.item in
      let mod_table = canonicalize_ast ident ast in
      merge_tables t mod_table
    | _ -> t
  in
  List.fold_left go Symboltable.empty ast

let rec apply_import t (i, span) =
  prerr_endline ("Import at line " ^ string_of_int i.i_kind.span.start.line);
  (* find file and remainder of path *)
  let file, path, mod_name = 
    let kind, full_path, path' = find_dir_or_file i.i_path in

    (* if we found a directory, we must be importing a file *)
    if kind = `Dir then begin
      let dir = full_path in
      let ident = match i.i_kind.item with
        | IK_Simple ident -> ident
        | _ ->
          let span = (List.hd (List.rev i.i_path)).span in
          cannot_import_from_dir_error dir span
      in
      let file_basename = Filename.concat dir ident.item in
      let file_path =
        (* if there was no dir we haven't checked
        in which search dir the imported file belongs *)
        if dir = "" then find_file file_basename ident.span
        else file_basename ^ Config.extension
      in
      file_path, path', ident.item
    end else
      full_path, path', (List.hd (List.rev i.i_path)).item
  in
  prerr_endline ("File: " ^ file);
  prerr_endline ("Module name: " ^ mod_name);
  
  (* parse and resolve the file *)
  let ast = !get_ast_fn file span in 
  prerr_endline "Parsed:";
  Parse.PrintAst.print_ast' 1 ast;
  let imported_table = canonicalize_ast (Ident.Ident mod_name) ast in
  print_ast_table imported_table;
  
  (* apply the path *)
  prerr_endline ("Remaining Path: " ^ String.concat "::" (nmapi path));
  
  prerr_newline ();
  t

and resolve mod_name (ast : ast) : ast_symbol_table =
  let table = canonicalize_ast mod_name ast in
  let imports =
    let rec go acc = function
      | {item = TL_Import i; span} :: tls -> go (acc @ [i, span]) tls
      | _ :: tls -> go acc tls 
      | [] -> acc
    in
    go [] ast
  in
  let try_apply_import t i =
    try apply_import t i
    with Report.Error e ->
      Report.report e;
      t
  in
  List.fold_left try_apply_import table imports