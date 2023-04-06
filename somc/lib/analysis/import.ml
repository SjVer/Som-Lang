open Report.Error
open Parse.Ast

let get_ast_from_file:
  (string -> Span.t -> ast) ref =
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
      "this directory is part of the standard library.\n\
      perhaps the import statement is mispelled?"
  in
  Report.add_note note report |> Report.raise

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

let decide_on_file_and_path imp =
  let kind, full_path, path' = find_dir_or_file imp.i_path in

  if kind = `Dir then begin
    let span = (List.hd (List.rev imp.i_path)).span in
    cannot_import_from_dir_error full_path span
  end else
    full_path, path'

(* import stuff *)

(* returns [context, imported_ast, main_ast] *)
let rec include_imports ast =
  (* let ( @* ) orig added =
    let is_dup tl = not (List.exists ((=) tl) orig) in
    orig @ List.filter is_dup added
  in *)

  let go ast_acc tl =
    match tl.item with
      | TLImport imp -> begin try
          let file, path = decide_on_file_and_path imp in
          let imp_ast = !get_ast_from_file file tl.span in

          let mod_name = Filename.(chop_extension (basename file)) in
          let mod_span = Span.concat_spans
            (List.hd imp_ast).span
            (List.hd (List.rev imp_ast)).span
          in
          (* prepend '#' to mark module as imported/hidden *)
          let mod_tl = TLModule ({tl with item = "#" ^ mod_name}, imp_ast) in

          (* NOTE: this might be causing issues later on *)
          let imp_path = {tl with item = mod_name} :: path in
          let imp_tl = TLImport {imp with i_path = imp_path} in

          ast_acc @ [{item=mod_tl; span=mod_span}; {tl with item=imp_tl}]
        with Report.Error r ->
          Report.report r;
          ast_acc
        end

      | TLModule (n, mod_ast) ->
        let mod_ast = include_imports mod_ast in
        let tl = {tl with item = TLModule (n, mod_ast)} in
        ast_acc @ [tl]

      | _ -> ast_acc @ [tl]
  in

  List.fold_left go [] ast