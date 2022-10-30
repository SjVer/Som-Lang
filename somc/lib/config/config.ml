module Cli = Cli

let _path = List.fold_left Filename.concat ""

(* internally used configs *)

let print_std_trees = true

let extension = ".som"
let include_dir =
  if Sys.unix then
    (* "/usr/share/som/include" *)
    "/home/sjoerd/Coding/Languages/Som/stdlib/include"
  else
    failwith ("unsupported os type: " ^ Sys.os_type)

let prelude_dir = _path [include_dir; "std"]
let prelude_ident = "prelude"
let prelude_file = _path [prelude_dir; prelude_ident] ^ ".som"

(* globals *)

let in_lsp_mode = ref false