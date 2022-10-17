module Log = (val Logs.src_log (Logs.Src.create "som-lsp"))

let setup_log () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  Log.info (fun f -> f "Initialized logging")

let rec loop io server =
  match Lsp.Io.read io with
    | Some (Jsonrpc.Message m) ->
      server.request
    | _ -> ()

let () =
  setup_log ();
  
  Log.info (fun f -> f "Starting server");
  (* let scheduler = Fiber_unix.Scheduler.create () in *)
  let io = Lsp.Io.make stdin stdout in
  let server = Language_server.server () in

  loop io server;

  Log.info (fun f -> f "Stopping server")
  Lsp.Io.close io