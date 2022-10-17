open Lsp

module JsonError = Jsonrpc.Response.Error
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

(** the 'entrypoint's for lsp requests *)

let process_request (type res) _client:
  res Client_request.t -> (res, JsonError.t) result Fiber.t =
  function
    | Initialize _ ->
      On_init.handle ()
      |> Result.map_error (fun msg -> JsonError.{
        code = InternalError;
        message = msg;
        data = None;
      }) |> Fiber.return
    | _ ->
      Log.err (fun f -> f "Unknown request");
      Error JsonError.{
        code = InternalError;
        message = "Unknown request";
        data = None
      } |> Fiber.return

let handle client req =
  try process_request client req
  with e ->
    let bt = Printexc.get_backtrace () in
    let e = Printexc.to_string e in
    Log.err (fun f -> f "Error handling request: %s\n%s" e bt);
    Error JsonError.{
      code = InternalError;
      message = e;
      data = None
    } |> Fiber.return