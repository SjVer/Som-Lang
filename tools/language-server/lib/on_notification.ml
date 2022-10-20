open Lsp

module JsonError = Jsonrpc.Response.Error
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

(** the 'entrypoint's for lsp notification *)

let worker _server: Client_notification.t -> unit =
  let open Client_notification in function
    | Unknown_notification n ->
      Log.warn (fun f -> f "Unknown notification '%s'" n.method_)
    | _ -> ()

let handle server noti =
  try worker server noti
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling notification: %s\n%s" e bt)