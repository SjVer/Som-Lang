open Lsp
open Server

let () =
  setup_log ();
  
  let io = Io.make stdin stdout in
  let server = make io in
  
  Log.info (fun f -> f "Starting server");
  run io server;

  Log.info (fun f -> f "Stopping server");
  Io.close io