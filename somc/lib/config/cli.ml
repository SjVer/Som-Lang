(* properties *)

let name = "somc"
let usage_msg = name ^ " [options] files..."
let version_msg = name ^ " 0.1.0"
let description = "Official Som compiler"

(* global config struct type *)

type args_t =
  {
    verbose: bool;
    mute: bool;
    opt_level: [`On | `O0 | `O1 | `O2 | `O3 | `Os | `Oz];
    passes: string list;
    search_dirs: string list;
    file: string;
  }

let args = ref {
    verbose=false;
    mute=false;
    opt_level=`O3;
    passes=[];
    search_dirs=[];
    file="";
  }