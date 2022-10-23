open Lsp
open Lsp.Types
open Util

module JsonError = Jsonrpc.Response.Error

let map_error = Result.map_error

(** the 'entrypoint's for lsp requests *)

let process_request (type res) server:
  res Client_request.t -> (res, JsonError.t) result =
  let open Client_request in function
    | Initialize _ ->
      On_init.handle () |> map_error internal_error_f
    | Shutdown -> Ok ()
    | TextDocumentHover p ->
      Requests.hover server p.textDocument.uri p.position
    | SemanticTokensFull p ->
      Requests.semantic_tokens server p.textDocument.uri
    | UnknownRequest r ->
      let msg = Util.fmt "Unknown request '%s'" r.meth in
      Error (internal_error_f msg)
    | _ -> Error (internal_error_f "Unknown request")

let handle server req =
  try process_request server req
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    T.Log.err (fun f -> f "Error handling request: %s\n%s" e bt);
    Error JsonError.{
      code = InternalError;
      message = e;
      data = None
    }