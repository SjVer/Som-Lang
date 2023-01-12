module Cli = Cli

let _path = List.fold_left Filename.concat ""

(* internally used configs *)

let force_tty = true

let print_std_trees = true

let extension = ".som"
let include_dir =
  if Sys.unix then
    (* "/usr/share/som/include" *)
    "/home/sjoerd/Coding/Languages/Som/stdlib"
  else
    failwith ("unsupported os type: " ^ Sys.os_type)

let prelude_ident = "prelude"
let prelude_import_path = ["std"; prelude_ident]
let prelude_file = _path (include_dir :: prelude_import_path) ^ ".som"

(* globals *)

let in_lsp_mode = ref false

let hide_stdlib_nodes = false