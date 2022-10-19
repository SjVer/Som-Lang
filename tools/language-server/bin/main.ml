open Lsp
open Server

let run () =
  setup_log ();
  
  let io = Io.make stdin stdout in
  let server = make io in
  
  Log.info (fun f -> f "Starting server");
  run io server;

  Log.info (fun f -> f "Stopping server");
  Io.close io

let show_version () =
  Printf.printf "%s %s\n" Sys.argv.(0) Somc.Config.Cli.version;
  exit 0

let () =
  let usage_msg = Sys.argv.(0) ^ " [--help|--version]" in
  Arg.parse [
    "--version", Arg.Unit show_version, "Prints version info";
  ] ignore usage_msg;
  run ()