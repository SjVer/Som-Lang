open Lsp
module JSonError = Jsonrpc.Response.Error

type client_chan = (* Store.client_channel = *)
  {
    notify: Server_notification.t -> unit Fiber.t;
    request: 'a. 'a Server_request.t -> ('a, JSonError.t) result Fiber.t;
  }

type server_chan =
  {
    request: 'r . client_chan -> 'r Client_request.t -> ('r, JSonError.t) result Fiber.t;
    notify: client_chan -> Client_notification.t -> unit Fiber.t;
  }

let server () : server_chan =
  {
    request = On_request.handle;
    notify = On_notification.handle;
  }