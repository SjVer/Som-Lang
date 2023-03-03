module C = Somc.Configs.Cli
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
  (* code, kind and name *)
  begin match Codes.error_name_from_code code with
    | Some (kind, name) ->
      Printf.printf "%s error E%03d: %s\n" kind code name;
    | None ->
      let open Error in
      Report.make_error (Other_error (Error.Cannot_explain code)) None
      |> Report.report;
      exit 1
  end;
  (* explanation *)
  begin match Codes.explanation_from_code code with
    | Some expl -> 
      print_newline ();
      print_endline expl;
      print_newline ();
      print_endline Configs.Cli.explain_help_message
    | None -> ()
  end;
  exit 0
      
(* cli parsing & entrypoint *)

let parseargs () =
  description C.description;

  (* cli stuff *)
  let verbose = flag
    ~set_long:"verbose"
    ~set_short:'v'
    ~description:"Produce verbose output"
    false in
  let compact = flag
    ~set_long:"compact"
    ~description:"Produce compact output"
    false in
  let mute = flag
    ~set_long:"mute"
    ~set_short:'m'
    ~description:"Mute all warnings and notes"
    false in
  let force_tty = flag
    ~set_long:"force-tty"
    ~description:"Force TTY output behaviour"
    false in

  (* output stuff *)
  let explain = optional_int
    ~long:"explain"
    ~placeholder:"CODE"
    ~description:"Explain the given error code"
    () in
  let print_ast = flag
    ~set_long:"print-ast"
    ~description:"Print the parsetree"
    false in
  let print_rast = flag
    ~set_long:"print-rast"
    ~description:"Print the resolved parsetree"
    false in
  let print_tast = flag
    ~set_long:"print-tast"
    ~description:"Print the typed parsetree"
    false in
  
  (* parse stuff *)
  let no_prelude = flag
    ~set_long:"no-prelude"
    ~description:"Don't implicitly include the prelude"
    false in
  let search_dirs = list_string
    ~long:"include"
    ~short:'i'
    ~placeholder:"DIR"
    ~description:"add DIR to the search directories"
    () in
  
  (* codegen stuff *)
  let passes = list_string
    ~long:"pass"
    ~short:'p'
    ~placeholder:"PASS"
    ~description:"Run pass PASS on the llvm IR"
    () in
  let opt_level = optional opt_typ
    ~short:'O'
    ~placeholder:"O3"
    ~description:"Set optimization level"
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

  (* act on args *)
  if force_tty then Report.enable_force_tty ();
  if Option.is_some explain then explain_ecode (Option.get explain);

  (* unwrap opt_level and map passes *)
  let opt_level' = match opt_level with Some o -> o | None -> `O3 in
  
  (* parse args and return struct *)
  close();
  C.args := C.{
    verbose;
    compact;
    mute;
    force_tty;

    file;
    print_ast;
    print_rast;
    print_tast;

    no_prelude;
    search_dirs;

    opt_level=opt_level';
    passes;
  }

(* entrypoint *)
let () =
  parseargs ();
  Symbols.reset ();
  let args = !(C.args) in

  if args.print_ast then
    let ast = Pipeline.ParseFile.call ((!C.args).file, None) in
    Parse.PrintAst.print_ast ast
  else if args.print_rast then
    let ast = Pipeline.AnalyzeFile.call ((!C.args).file, None, None) in
    Parse.PrintAst.print_ast ast
  else if args.print_tast then
    Typing.PrintTAst.print_tast (
      Pipeline.TypecheckFile.call
        (!C.args).file)
  else
    ignore (
      Pipeline.TypecheckFile.call
        (!C.args).file);

  exit 0