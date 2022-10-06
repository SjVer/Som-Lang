type im = Parse.Ast.import
type imk = Parse.Ast.import_kind

(* 
  brainstorming import algorithm:

    statement: `#dir1/dir2/file::section::symbol`
    import: {
      path: ["dir1", "dir2", "file", "section", "symbol"]
      kind: simple
    }

    step 1:
      find the longest real file path
      starting from the head of the path
      -> "dir1/dir2/file"

    step 2:
      parse file and get its symbols
      -> "file" contains [..., "section", ...]

    step 3:
      resolve the rest of the path
      -> "section" contains "symbol"

    step 4:
      handle import kind
      -> kind = simple, so import last segment "symbol"
*)

let get_file_or_dir path =
  let rec go = function
    | [] -> raise Not_found
    | t :: tl ->
      let start = match t with
        | `Stdlib -> Config.include_dir
        | `Cwd -> Sys.getcwd ()
      in
      let dir = Filename.concat start path in
      print_endline dir;

      if Sys.is_directory dir then `Dir dir
      else begin
        let file = dir ^ ".som" in
        if Sys.file_exists file then `File file
        else go tl
      end
  in go [`Cwd; `Stdlib]

let rec resolve_file acc = function
  | (hd :: tl) as path -> begin
      try
        let segment = hd.Parse.Ast.item in
        match get_file_or_dir (Filename.concat acc segment) with
          | `Dir d -> resolve_file d tl
          | `File f -> 
            let path' = List.map (fun p -> p.Parse.Ast.item) path in
            f, Path.from_list path'
      with Not_found ->
        let path' = List.map (fun p -> p.Parse.Ast.item) path in
        acc, Path.from_list path'
    end
  | _ -> acc, Path.from_list []

let import ({path; kind=_} : im) =
  let file, path' = resolve_file "" path in
  print_string "file: ";
  print_endline file;
  print_string "path: ";
  print_endline (Path.to_string path');