module Cli = Cli

let _path = List.fold_left Filename.concat ""

let _unsupported () =
  failwith ("unsupported OS type: " ^ Sys.os_type)

let _ocaml_config =
  let t = Configurator.V1.create "somc-ocaml-config" in
  fun key ->
    match Configurator.V1.ocaml_config_var t key with
      | Some value -> value
      | None -> failwith ("could not get config: " ^ key)

(* internally used configs *)

let extension = ".som"
let include_dir =
  if Sys.unix then
    (* "/usr/share/som/include" *)
    "/home/sjoerd/Coding/Languages/Som/stdlib"
  else _unsupported ()

let prelude_name = "prelude"
let prelude_ident = ["std"; prelude_name]
let prelude_file = _path (include_dir :: prelude_ident) ^ ".som"

let ld_path = _ocaml_config "c_compiler"
let ld_args = ["-lsom"; "-lsom-jemalloc"]

let exe_ext = _ocaml_config "ext_exe"
let obj_ext = _ocaml_config "ext_obj"

(* globals *)

let force_tty = true
let in_lsp_mode = ref false
let hide_stdlib_nodes = false
let tast_print_debug = true
let maximum_tag = 251
