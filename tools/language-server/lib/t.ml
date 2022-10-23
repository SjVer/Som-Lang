open Lsp
module JSonError = Jsonrpc.Response.Error

module Log = (val Logs.src_log (Logs.Src.create "som-language-server"))

type client =
  {
    (* request: 'a. 'a Server_request.t -> ('a, JSonError.t) result Fiber.t; *)
    notify: Server_notification.t -> unit;
  }

type server =
  {
    request: 'r . server -> 'r Client_request.t -> ('r, JSonError.t) result;
    notify: server -> Client_notification.t -> unit;
    client: client;
    store: Store.t;
  }