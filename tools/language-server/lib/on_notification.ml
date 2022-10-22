open Lsp

module JsonError = Jsonrpc.Response.Error
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

(** the 'entrypoint's for lsp notification *)

let worker (server: T.server): Client_notification.t -> unit =
  let open Client_notification in function
    | TextDocumentDidOpen p ->
      if p.textDocument.languageId = "som" then
        let doc = Store.add server.store (Text_document.make p) in
        let uri = Text_document.documentUri doc in
        Notifications.publish_diagnostics server uri
      else ()
    | TextDocumentDidChange p ->
      let uri = p.textDocument.uri in
      let version = p.textDocument.version in
      let doc = Store.get_doc server.store uri in
      let doc' = List.fold_left
        (Text_document.apply_content_change ~version)
        doc p.contentChanges
      in
      Store.set_doc server.store uri doc';
      Notifications.publish_diagnostics server uri;
    | TextDocumentDidClose p ->
      Store.close server.store p.textDocument.uri
    | Unknown_notification n ->
      Log.warn (fun f -> f "Unknown notification '%s'" n.method_)
    | _ -> ()

let handle server noti =
  try worker server noti
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling notification: %s\n%s" e bt)