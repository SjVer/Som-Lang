open Parse.Ast

let get_ast_fn = ref
  (fun (""|_): ast -> assert false)

(* 
  import algorithm:

    statement: `#dir1/dir2/file::section::symbol`
    import: {
      path: ["dir1", "dir2", "file", "section", "symbol"]
      kind: simple
    }

    step 1: (get_file_or_dir & resolve_file)
      find the longest real file path
      starting from the head of the path
      -> "dir1/dir2/file"

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

let get_file_or_dir path =
  let go prefix =
      let dir = Filename.concat prefix path in
      let file = (dir ^ Config.extension) in
      
      try
        if Sys.is_directory dir then `Dir path
        else assert false
      with Sys_error _ ->
        if Sys.file_exists file
          then `File file
          else `Not_found
  in
  
  match go (Sys.getcwd ()) with
    | `Not_found -> go Config.include_dir
    | _ as r -> r 

let rec resolve_file acc = function
  | (hd :: tl) as path -> begin
      let segment = hd.Parse.Ast.item in
      match get_file_or_dir (Filename.concat acc segment) with
        | `Dir d -> resolve_file d tl
        | `File f -> f, tl
        | `Not_found -> acc, path
    end
  | [] ->
    (* found a directory, not a file *)
    let open Report.Error in
    raise_error (Other_error (Cannot_import_dir acc)) None []

(** returns a function that when given
    a string returns a toplevel item
    with that name (section or ...) *)
let resolve_symbol file =
  let get_name = function
    | TL_Definition b -> (match b.patt.item with
      | PA_Variable n -> n
      | _ -> "")
    | TL_Section (n, _) -> n
    | _ -> ""
  in

  let go tast = function
    | [] -> fun n -> TL_Section (n, tast)
    | [n] ->
        (* find [n] in ast's symbols *)
        let rec find = function
          | hd :: tls ->
            let name = get_name hd.Parse.Ast.item in
            if n = name then hd
            else find tls
          | [] -> raise Not_found
        in
        
        let tl = find tast in
        begin fun n' ->
          if n' = n then tl.item
          else TL_Link (n', tl)
        end 

    | _ -> failwith ""

  in go (!get_ast_fn file)

let desugar ({path; kind} : import) =
  let file, path' = resolve_file "" path in
  let path'' = List.map (fun n -> n.item) path' in

  (* TODO: allow importing directories? *)

  let report () =
      let fst = List.hd path' in
      let lst = List.hd (List.rev path') in
      let span = Span.concat_spans fst.span lst.span in
      let path_str = String.concat "::" path'' in

      let open Report.Error in
      let e = Failed_to_resolve path_str in
      raise_error (Type_error e) (Some span) []
  in
  
  if file = "" then report ();
  let symbol =
    try resolve_symbol file path''
    with Not_found -> report ()
  in

  match kind.item with
    | IK_Simple ->
      let name = List.hd (List.rev path) in
      symbol name.item
    | IK_Rename n -> symbol n
      
    | _ -> failwith "IK"
  