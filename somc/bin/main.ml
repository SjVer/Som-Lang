module C = Somc.Config.Cli
open Clap

(* helper functions *)

let has_arg long short = Array.exists (fun a -> a = long || a = short) Sys.argv
let exit_after () = exit 0

(* opt enum type for cli (idk why but for some reason the prefix '-' is needed) *)
let opt_typ = enum "" [
  "-0", `O0; "-1", `O1; "-2", `O2; "-3", `O3;
  "-n", `On; "-s", `Os; "-z", `Oz
  ]

(* explain given error code *)
let explain_ecode code =
  let open Report in
  match Codes.error_name_from_int code with
  | Some (kind, name) ->
    Printf.printf "%s error E%03d:%s\n" kind code name;
    exit 0
  | None ->
    let open Error in
    Report.make_error (Other_error (Error.Cannot_explain code)) None
    |> Report.report;
    exit 1
      
(* cli parsing & entrypoint *)

let () =
  description C.description;

  (* named args *)
  let verbose = flag
    ~set_long:"verbose"
    ~set_short:'v'
    ~description:"Produce verbose output"
    false in
  let mute = flag
    ~set_long:"mute"
    ~set_short:'m'
    ~description:"Mute all warnings"
    false in
  let opt_level = optional opt_typ
    ~short:'O'
    ~placeholder:"O3"
    ~description:"Set optimization level"
    () in
  let explain = optional_int
    ~long:"explain"
    ~placeholder:"CODE"
    ~description:"Explain the given error code"
    () in
  let passes = list_string
    ~long:"pass"
    ~short:'p'
    ~placeholder:"PASS"
    ~description:"Run pass PASS on the llvm IR"
    () in
  let search_dirs = list_string
    ~long:"include"
    ~short:'i'
    ~placeholder:"DIR"
    ~description:"add DIR to the search directories"
    () in
  
  (* unnamed args *)
  let file = mandatory_string
    ~placeholder:"FILE"
    ~description:"File to compile"
    () in
  
  (* check hidden args *)
  if has_arg "--help" "-h"    then exit_after (help ());
  if has_arg "--usage" "-u"   then exit_after (print_endline C.usage_msg);
  if has_arg "--version" "-V" then exit_after (print_endline C.version_msg);

  (* check --explain *)
  if Option.is_some explain then explain_ecode (Option.get explain);

  (* unwrap opt_level and map passes *)
  let opt_level' = match opt_level with Some o -> o | None -> `O3 in
  
  (* parse args and return struct *)
  close();
  C.args := C.{
    verbose;
    mute;
    opt_level=opt_level';
    passes;
    search_dirs;
    file;
  }

(* entrypoint *)
let () =
  if !(C.args).verbose then
    Typing.PrintTAst.print_tast (
      Pipeline.TypecheckFile.call
        (!C.args).file)
  else
    ignore (
      Pipeline.TypecheckFile.call
        (!C.args).file);
  exit 0