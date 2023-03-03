open Report.Error
open Parse.Ast
open Context

type ast_symbol_table = Context.ast_symbol_table

let get_ast_symbol_table:
  (string -> Ident.t -> Span.t -> ast_symbol_table) ref =
  ref (fun _ -> assert false)

(* error helper functions *)

let file_not_found_error path =
  let e = Failed_to_import (path.item ^ Configs.extension) in
  Report.make_error (Other_error e) (Some path.span)
  |> Report.add_note (Printf.sprintf
    "try adding directory '%s%s' or\n\
    file '%s%s' to the search paths."
    path.item Filename.dir_sep path.item Configs.extension)
  |> Report.raise

let cannot_import_from_dir_error dir span =
  let err = Cannot_import_dir (dir ^ Filename.dir_sep) in
  let report = Report.make_error (Other_error err) (Some span) in
  let note =
    if not (Span.is_in_stdlib {span with file = dir}) then
      Printf.sprintf
        "try adding file '%s%s'\n\
        and exposing any sub-modules there."
        dir Configs.extension
    else
      "This directory is part of the standard library.\n\
      Perhaps the import statement is mispelled?"
  in
  Report.add_note note report |> Report.raise

let mod_has_no_symbol_error mident what sname =
  let e = Has_no_symbol (Ident.to_string mident, what, sname.item) in
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
  else if Sys.file_exists (full_path ^ Configs.extension) then `File
  else raise Not_found

let rec recurse_in_dir dir = function
  | hd :: tl -> begin
      let full_path = Filename.concat dir hd.item in
      match check_full_path_type full_path with 
        | `Dir -> recurse_in_dir full_path tl
        | `File -> `File, full_path ^ Configs.extension, tl
    end
  | [] -> `Dir, dir, []

let find_dir_or_file path =
  let search_dirs =
    "" (* CWD *)
    :: !(Configs.Cli.args).search_dirs
    @ [Configs.include_dir]
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

let decide_on_file_and_path imp =
  let kind, full_path, path' = find_dir_or_file imp.i_path in

  (* if we found a directory, we must be importing a file *)
  if kind = `Dir then begin
    (* let dir = full_path in
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
      else file_basename ^ Configs.extension
    in
    file_path, path', true *)
    let span = (List.hd (List.rev imp.i_path)).span in
    cannot_import_from_dir_error full_path span
  end else
    full_path, path', false

(* import stuff *)

let import_symbol src_ctx src_path dest_ctx dest_ident =
  (* NOTE: only changing bindings *)
  let src_ident = Ident.from_list (nmapi src_path) in
  try
    let qual_ident = lookup_qual_value_ident src_ctx src_ident in
    Context.bind_qual_value_ident dest_ctx dest_ident qual_ident
  with Not_found -> try
    let qual_ident = lookup_qual_type_ident src_ctx src_ident in
    Context.bind_qual_type_ident dest_ctx dest_ident qual_ident
  with Not_found ->
    let span = Span.concat_spans
      (List.hd src_path).span
      (List.hd (List.rev src_path)).span
    in
    let src_ident' = {item = Ident.to_string src_ident; span} in
    mod_has_no_symbol_error src_ctx.name "symbol" src_ident'

let rec apply_import_kind dest_ctx mod_ctx kind =
  match kind.item with
    | IK_Module ->
      (* preprend all bindings in the module *)
      let f k d m = IMap.add (Ident.prepend mod_ctx.name k) d m in
      let value_map = IMap.fold f mod_ctx.value_map dest_ctx.value_map in
      let type_map = IMap.fold f mod_ctx.type_map dest_ctx.type_map in
      {dest_ctx with value_map; type_map}

    | IK_Simple p ->
      (* use `a::b::c` import `c`, not `a::b::c` *)
      let ident = Ident.Ident (List.hd (List.rev p)).item in
      import_symbol mod_ctx p dest_ctx ident

    | IK_Glob ->
      (* copy over all bindings *)
      let value_map = IMap.fold IMap.add mod_ctx.value_map dest_ctx.value_map in
      let type_map = IMap.fold IMap.add mod_ctx.type_map dest_ctx.type_map in
      {dest_ctx with value_map; type_map}

    | IK_Rename (p, n) ->
      let new_ident = Ident.Ident n.item in
      import_symbol mod_ctx p dest_ctx new_ident
      
    | IK_Nested kinds ->
      let f ctx' kind' = apply_import_kind ctx' mod_ctx kind' in
      List.fold_left f dest_ctx kinds

let apply_import ctx (imp, span) =
  (* find file and remainder of path *)
  let file, path, _importing_file = decide_on_file_and_path imp in
  let imp_mod_name = Filename.(chop_extension (basename file)) in
  
  (* parse and get symbols from the file *)
  let imp_mod_ident = Ident.append ctx.name (Ident imp_mod_name) in
  let imp_table = !get_ast_symbol_table file imp_mod_ident span in

  (* merge tables AND bindings temporarily *)
  let tmp_ctx = Context.add_table ctx imp_table true in
  (* the final context will not need all the bindings *)
  let ret_ctx = Context.add_table ctx imp_table false in
  
  let mod_path =
    if path = [] then imp_mod_ident
    else Ident.append imp_mod_ident (Ident.from_list (nmapi path))
  in
  let mod_ctx = Context.extract_subcontext tmp_ctx mod_path in
  apply_import_kind ret_ctx mod_ctx imp.i_kind

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
