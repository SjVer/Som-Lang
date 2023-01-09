open Parse.Ast
open Report.Error

let ext = Config.extension

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

let file_not_found_error path =
  let e = Failed_to_import (path.item ^ ext) in
  Report.make_error (Other_error e) (Some path.span)
  |> Report.add_note (Printf.sprintf
    "try adding directory '%s/' or\n\
    file '%s.som' to the search paths."
    path.item path.item)
  |> Report.raise

let failed_to_resolve_error node =
  let e = Failed_to_resolve node.item in
  Report.make_error (Type_error e) (Some node.span)
  |> Report.raise

let sect_has_no_sect_error sname name =
  let e = Has_no_section (sname, name.item) in
  Report.make_error (Type_error e) (Some name.span)
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

let extract_sect from = function
  | TL_Module (_, ast) -> ast
  | _ ->
    let e = Cannot_import_from from.item in
    Report.make_error (Type_error e) (Some from.span)
    |> Report.raise

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

let resolve_section fname ast =
  let rec find_sect_in_ast sname name = function
    | hd :: tl ->
      begin match hd.item with
        | TL_Module (n, ast)
          when n = name -> ast
        | _ -> find_sect_in_ast sname name tl
      end
    | [] -> sect_has_no_sect_error sname name 
  in

  let rec go sname ast = function
    | [] -> fname, ast
    | [n] ->
        check_import_private n;
        n, find_sect_in_ast sname n ast
    | n :: ns ->
      let sect = find_sect_in_ast sname n ast in
      go n.item sect ns
  in go fname.item ast

let find_symbol_in_sect ast name =
  let get_name_from_tl = function
    | TL_Definition b -> (match b.patt.item with
      | PA_Variable n -> n
      | _ -> "")
    | TL_Type_Definition d -> d.name.item
    | TL_Module (n, _) -> n.item
    | _ -> ""
  in
  let rec find_in_ast n = function
    | tl :: tls ->
      let name = get_name_from_tl tl.item in
      if n.item = name then tl
      else find_in_ast n tls
    | [] -> failed_to_resolve_error n 
  in

  let tl = find_in_ast name ast in
  check_import_private name;
  tl

(* main stuff *)

let resolve_import names ({path; kind} : import) s =
  let file, path' = "", path in

  let sect_name, sect =
    let fname = List.hd path in
    let ast = !get_ast_fn file s in
    resolve_section fname ast path'
  in

  let check_shadowing n =
    match List.find_opt ((=) n) !names with
      | Some n -> shadowed_symbol_warning n s
      | None -> names := (n :: !names)
  in

  let rec finish sect = function
    | IK_Simple n ->
      let s = find_symbol_in_sect sect n in
      check_shadowing n.item;
      [s.item]
    | IK_Rename (on, nn) ->
      let s = find_symbol_in_sect sect on in
      let s' = match s.item with
        | TL_Type_Definition d -> TL_Type_Definition {d with name=nn}
        | TL_Module (_, ast) -> TL_Module (nn, ast)
        | _ -> invalid_arg "TODO"
      in
      check_shadowing nn.item;
      [s']
    | IK_Nested is ->
      let go inode =
        let path, kind = inode.item.path, inode.item.kind.item in
        let _, subsect = resolve_section sect_name sect path in
        finish subsect kind
      in
      List.flatten (List.map go is)
    | IK_Glob ->
      List.map (fun n -> n.item) sect
  in finish sect kind.item

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
      []
  in
  let rec go' ast = function
    | [] -> ast
    | tl :: tls -> go' (ast @ go tl) tls
  in go' []