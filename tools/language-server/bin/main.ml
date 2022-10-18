open Lsp
open Language_server

module Log = (val Logs.src_log (Logs.Src.create "som-lsp"))

let setup_log () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  Log.info (fun f -> f "Initialized logging")

let () =
  setup_log ();
  
  Log.info (fun f -> f "Starting server");
  (* let scheduler = Fiber_unix.Scheduler.create () in *)
  let io = Io.make stdin stdout in
  let server = server io in

  run_server io server;

  Log.info (fun f -> f "Stopping server");
  Io.close io