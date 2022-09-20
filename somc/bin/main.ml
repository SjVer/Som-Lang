open Clap
open Somc.Codegen.Opt

module R = Somc.Report
module E = Somc.Report.Error
module C = Somc.Report.Codes

(* information *)
let usage_msg = "somc [--verbose|-v] [--mute|-m] <FILE>"
let version_msg = "somc 0.1.0"

(* helper functions *)
let has_arg long short = Array.exists (fun a -> a = long || a = short) Sys.argv
let exit_after () = exit 0

(* opt enum type for cli (idk why but for some reason the prefix '-' is needed) *)
let opt_typ = enum "optimization level" ["-0", O0; "-1", O1; "-2", O2; "-3", O3; "-s", Os; "-z", Oz]

(* explain given error code *)
let explain_ecode code =
  match C.error_name_from_int code with
  | Some (kind, name) ->
    Printf.printf "%s error E%03d: %s\n" kind code name;
    exit 0
  | None ->
    R.report (E.Other_error (E.Cannot_explain code)) None [];
    exit 1

(* entrypoint *)
let () =
  description "Official Som compiler";

  (* named args *)
  let verbose   = flag ~set_long: "verbose" ~set_short: 'v' ~description: "Produce verbose output"  false in
  let mute      = flag ~set_long: "mute"    ~set_short: 'm' ~description: "Mute all warnings"       false in
  let opt_level = optional opt_typ ~short: 'O' ~placeholder: "O3" ~description: "Set optimization level" () in
  let explain   = optional_int ~long: "explain" ~placeholder: "CODE" ~description: "Explain the given error code" () in
  
  (* unnamed args *)
  let file = mandatory_string ~placeholder: "FILE" ~description: "File to compile" () in
  
  (* check hidden args *)
  if has_arg "--help" "-h"    then exit_after (help ());
  if has_arg "--usage" "-u"   then exit_after (print_endline usage_msg);
  if has_arg "--version" "-V" then exit_after (print_endline version_msg);

  (* check --explain *)
  if Option.is_some explain then explain_ecode (Option.get explain);

  (* unwrap opt_level Option *)
  (* let opt_level' = match opt_level with Some o -> o | None -> O3 in *)
  
  (* parse args and do main stuff *)
  close();
  ignore (verbose, mute, opt_level);

  Somc.Parse.PrintAst.print_toplevel (Somc.Parse.parse file);
  exit 0