(* properties *)
let name = "somc"

let version = "0.1.0"
let usage_msg = name ^ " [options] files..."
let version_msg = name ^ " " ^ version
let description = "Official Som compiler"

let explain_help_message = "For more information go to https://sjver.github.io/Som-Lang."

(* global config struct type *)

type args_t =
  {
    verbose: bool;
    compact: bool;
    mute: bool;
    force_tty: bool;

    file: string;
    output: string option;
    output_asm: bool;
    output_obj: bool;
    dry_run: bool;

    dump_ast: bool;
    dump_rast: bool;
    dump_tast: bool;
    dump_raw_ir: bool;
    dump_ir: bool;
    dump_raw_llvm: bool;
    dump_llvm: bool;

    search_dirs: string list;
    no_prelude: bool;

    opt_level: [`On | `O0 | `O1 | `O2 | `O3 | `Os | `Oz];
    passes: string list;
    target: string option;
  }

let args = ref {
    verbose = false;
    compact = false;
    mute = false;
    force_tty = false;
    
    file = "";
    output = None;
    output_asm = false;
    output_obj = false;
    dry_run = false;

    dump_ast = false;
    dump_rast = false;
    dump_tast = false;
    dump_raw_ir = false;
    dump_ir = false;
    dump_raw_llvm = false;
    dump_llvm = false;

    search_dirs = [];
    no_prelude = false;

    opt_level = `O3;
    passes = [];
    target = None;
  }
