module Cli = Cli

let _path = List.fold_left Filename.concat ""

(* internally used configs *)

let force_tty = true

let extension = ".som"
let include_dir =
  if Sys.unix then
    (* "/usr/share/som/include" *)
    "/home/sjoerd/Coding/Languages/Som/stdlib"
  else
    failwith ("unsupported os type: " ^ Sys.os_type)

let prelude_name = "prelude"
let prelude_ident = ["std"; prelude_name]
let prelude_file = _path (include_dir :: prelude_ident) ^ ".som"

(* globals *)

let in_lsp_mode = ref false

let hide_stdlib_nodes = false

let tast_print_debug = true

let minimum_tag = 4
