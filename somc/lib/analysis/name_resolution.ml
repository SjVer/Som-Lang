open Parse.Ast
open Report.Error
open Symboltable
open Scope
open Ident

let get_ast_symbol_table:
  (string -> Ident.t -> Span.t -> ast_symbol_table) ref =
  ref (fun _ -> assert false)

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

let rec extract_submodule scope mod_name = function
  | hd :: tl ->
    if not (check_submodule scope.table hd.item) then
      let e = Type_error (Has_no_symbol (mod_name, "submodule", hd.item)) in
      Report.make_error e (Some hd.span)
      |> Report.raise
    else
      let scope' = Scope.extract_prefixed scope hd.item in
      extract_submodule scope' hd.item tl
  | [] -> scope, mod_name
    
let find_and_add_value_or_type src old_ident dest new_ident =
  (* TODO: allow importing both *)
  (* TODO: just add a binding in the map instead of copying the entry *)
  (* TODO: allow importing submodules *)
  let go f entry = 
    let table = f dest.table new_ident entry in
    {dest with table}
  in
  try
    go add_value_entry (get_value src.table old_ident)
  with Not_found ->
    go add_type_entry (get_type src.table old_ident)

let apply_import scope (imp, span) =
  (* find file and remainder of path *)
  let file, path, importing_file = decide_on_file_and_path imp in
  let imp_mod_name = Filename.(chop_extension (basename file)) in
  
  (* parse and get symbols from the file *)
  let imp_mod_ident = Ident.append_opt scope.name (Ident imp_mod_name) in
  let imp_table = !get_ast_symbol_table file imp_mod_ident span in
  let imp_scope =
    {
      (Scope.empty (append_opt scope.name (Ident imp_mod_name)))
      with table = imp_table
    }
  in
  print imp_scope;

  (* merge tables to preserve all symbols *)
  let scope' = Scope.add_table scope imp_table in
  
  (* allow nested imports *)
  let rec finish dests srcs mod_name path kind : scope =
    (* apply the path *)
    let srcs', mod_name' = extract_submodule srcs mod_name path in
    print srcs';
    
    (* apply the import kind *)
    match kind.item with
      | IK_Simple n -> begin
          let old_ident = from_list (nmapi path @ [n.item]) in
          let new_ident = Ident n.item in
          try find_and_add_value_or_type srcs' old_ident dests new_ident
          with Not_found -> mod_has_no_symbol_error mod_name' "symbol" n
        end
      | IK_Glob ->
        let imp_scope' = extract_prefixed imp_scope mod_name in
        Scope.merge_scopes dests imp_scope'
      | IK_Rename (on, nn) -> begin
          let old_ident = from_list (nmapi path @ [on.item]) in
          let new_ident = Ident nn.item in
          try find_and_add_value_or_type srcs' old_ident dests new_ident
          with Not_found -> mod_has_no_symbol_error mod_name' "symbol" on
        end
      | IK_Nested imps ->
        let f scope'' imp = finish dests scope'' mod_name' imp.i_path imp.i_kind in
        List.fold_left f srcs' (nmapi imps)
  in

  (* if we're importing a file the IK_Simple is already handled *)
  if importing_file then
    scope' (* the tables are already merged *)
  else
    finish scope' imp_scope imp_mod_name path imp.i_kind

let gather_and_apply_imports scope ast =
  (* gather imports *)
  let imports =
    let rec go acc = function
      | {item = TL_Import i; span} :: tls -> go (acc @ [i, span]) tls
      | _ :: tls -> go acc tls
      | [] -> acc
    in
    go [] ast
  in

  (* apply imports *)
  let try_apply_import s i =
    try apply_import s i
    with Report.Error e -> Report.report e; s
  in
  List.fold_left try_apply_import scope imports

let resolve mod_ident is_imp ast : ast_symbol_table =
  let scope =
    if is_imp then Scope.empty mod_ident
    else Scope.nameless_empty
  in
  let scope' = gather_and_apply_imports scope ast in
  let scope'' = Canonicalize.canon_ast scope' ast in
  scope''.table