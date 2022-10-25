open Parse.Ast
open Report.Error

let get_ast_fn:
  (string -> Span.t -> ast) ref =
  ref (fun _ _ : ast -> assert false)

(* 
  import algorithm:
    statement: `#dir1/dir2/file::section::symbol`
    import: {
      dir: ["dir1", "dir2"]
      path: ["file", "section", "symbol"]
      kind: simple
    }
    step 1: (get_file_or_dir & resolve_file)
      find the longest real file path
      starting from the head of the path
      -> "dir1/dir2/file.som"
    step 2: (resolve_symbol)
      parse file and get its symbols
      -> "file" contains [..., "section", ...]
    step 3: (resolve_symbol)
      resolve the rest of the path
      -> "section" contains "symbol"
    step 4: (import)
      handle import kind
      -> kind = simple, so import last segment "symbol"
*)

(* helper functions *)

let file_not_found_error node =
  let e = Failed_to_import (node.item ^ Config.extension) in
  Report.make_error (Other_error e) (Some node.span)
  |> Report.add_note (Printf.sprintf
      "try adding directory '%s/' or\n\
      file '%s.som' to the search paths."
      node.item node.item)
  |> Report.raise

let symbol_not_found_error node =
  let e = Failed_to_resolve node.item in
  Report.make_error (Type_error e) (Some node.span)
  |> Report.raise

let shadowed_symbol_warning n s =
  let msg = Printf.sprintf "import shadows previous import of `%s`" n in
  Report.make_warning msg (Some s)
  |> Report.report

let check_import_private n s =
  if n.[0] = '_' then
    let e = Cannot_private ("import", n) in
    Report.make_error (Type_error e) (Some s)
    |> Report.report

let extract_sect from = function
  | TL_Section (_, ast) -> ast
  | _ ->
    let e = Cannot_import_from from.item in
    Report.make_error (Type_error e) (Some from.span)
    |> Report.raise

(* finding files and resolving symbols *)

let resolve_file dir = function
  | [] -> failwith "Name_res.resolve_file"
  | phd :: ptl ->
    let dir' = List.fold_left Filename.concat "" dir in
    let search_dirs =
      (* TODO: allow disabling stdlib *)
      "" (* cwd *)
      :: !(Config.Cli.args).search_dirs
      @ [Config.include_dir]
    in

    (* executed per include path *)
    let rec go = function
      | [] -> file_not_found_error phd
      | d :: ds ->
        let file = Filename.concat
          (Filename.concat d dir')
          phd.item ^ Config.extension
        in
        if Sys.file_exists file then file
        else go ds
    in
    go search_dirs, ptl

(** returns a function that when given
    a string returns a toplevel item
    with that name (section or ...) *)
let resolve_symbol span ast =
  let get_name_from_tl = function
    | TL_Definition b -> (match b.patt.item with
      | PA_Variable n -> n
      | _ -> "")
    | TL_Type_Definition d -> d.name.item
    | TL_Section (n, _) -> n.item
    | _ -> ""
  in
  let rec find_in_tls n = function
    | hd :: tls ->
      let name = get_name_from_tl hd.item in
      if n.item = name then hd
      else find_in_tls n tls
    | [] -> symbol_not_found_error n 
  in

  let rec go ast = function
    | [] -> fun n ->
        let n' = {item = n; span} in
        TL_Section (n', ast)
    | [n] ->
        check_import_private n.item n.span;
        let tl = find_in_tls n ast in
        begin fun n' ->
          if n' = n.item then tl.item
          else TL_Link (n', tl)
        end
    | n :: ns ->
      let sect = find_in_tls n ast in
      let sect' = extract_sect n sect.item in
      go sect' ns
  in go ast

(* main stuff *)

let resolve_import names ({dir; path; kind} : import) s =
  (* TODO: allow importing directories? *)
  let file, path' = resolve_file (nmapi dir) path in

  let ast = !get_ast_fn file s in
  let sym = resolve_symbol s ast path' in

  let add_and_check_name n =
    match List.find_opt ((=) n) !names with
      | Some n -> shadowed_symbol_warning n s
      | None -> names := (n :: !names)
  in

  let rec finish path symbol_w_name = function
    | IK_Simple ->
      let name = List.hd (List.rev path) in
      add_and_check_name name.item;
      [symbol_w_name name.item]

    | IK_Rename n ->
      add_and_check_name n;
      [symbol_w_name n]

    | IK_Nested is ->
      let n = List.hd (List.rev path) in
      let ast = extract_sect n (symbol_w_name "") in
      let go n =
        let path, kind = n.item.path, n.item.kind.item in
        let symbol_w_name' = resolve_symbol n.span ast path in
        finish path symbol_w_name' kind
      in
      List.flatten (List.map go is)
      
    | IK_Glob ->
      let n = List.hd (List.rev path) in
      let ast = extract_sect n (symbol_w_name "") in
      List.map (fun n -> n.item) ast
  in finish path sym kind.item

let resolve =
  let names = ref [] in
  (* execute on each toplevel node *)
  let go (node: toplevel node) =
    let ret_gh i =
      {
        span={node.span with ghost=true};
        item=i;
      }
    in
    try match node.item with
      | TL_Import i ->
        let new_tls = resolve_import names i node.span in
        List.map ret_gh new_tls
      | _ -> [node]
    with Report.Error e ->
      Report.report e;
      [node]
  in
  let rec go' ast = function
    | [] -> ast
    | tl :: tls -> go' (ast @ go tl) tls
  in go' []