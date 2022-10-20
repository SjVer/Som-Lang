open Lsp
module JSonError = Jsonrpc.Response.Error

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
    (* store: Store.t; *)
  }