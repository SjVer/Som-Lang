(* properties *)

let name = "somc"
let version = "0.1.0"
let usage_msg = name ^ " [options] files..."
let version_msg = name ^ " " ^ version
let description = "Official Som compiler"

(* global config struct type *)

type args_t =
  {
    verbose: bool;
    mute: bool;

    file: string;
    print_ast: bool;
    print_resolved_ast: bool;
    print_tast: bool;

    search_dirs: string list;
    no_prelude: bool;

    opt_level: [`On | `O0 | `O1 | `O2 | `O3 | `Os | `Oz];
    passes: string list;
    
  }

let args = ref {
    verbose = false;
    mute = false;
    
    file = "";
    print_ast = false;
    print_resolved_ast = false;
    print_tast = false;

    search_dirs = [];
    no_prelude = false;

    opt_level = `O3;
    passes = [];
  }