open Lsp
open Lsp.Types
open Util

module JsonError = Jsonrpc.Response.Error
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

let map_error = Result.map_error

(** the 'entrypoint's for lsp requests *)

let process_request (type res) server:
  res Client_request.t -> (res, JsonError.t) result =
  let open Client_request in function
    | Initialize _ ->
      On_init.handle ()
      |> map_error internal_error_f
    | TextDocumentHover {textDocument = {uri}; position} ->
      Textdoc_methods.hover server (uri_from_docuri uri) position
    | _ -> Error (internal_error_f "Unknown request")

let handle server req =
  try process_request server req
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling request: %s\n%s" e bt);
    Error JsonError.{
      code = InternalError;
      message = e;
      data = None
    }