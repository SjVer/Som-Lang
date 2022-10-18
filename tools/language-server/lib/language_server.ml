open Lsp
module JSonError = Jsonrpc.Response.Error

(* type stuff *)

type client =
  {
    (* request: 'a. 'a Server_request.t -> ('a, JSonError.t) result Fiber.t; *)
    notify: Server_notification.t -> unit;
  }

type server =
  {
    request: 'r . client -> 'r Client_request.t -> ('r, JSonError.t) result;
    notify: client -> Client_notification.t -> unit;
    client: client;
  }

let client io : client =
  (* let request r =
    let id = `Int 0 in
    let r' = Server_request.to_jsonrpc_request r ~id in
    Io.send io (Jsonrpc.Message {r' with id = Some r'.id});
    Fiber.return (Ok )
  in *)
  let notify n =
    let n' = Server_notification.to_jsonrpc n in
    Io.send io (Jsonrpc.Message {n' with id = None})
  in
  {
    (* request; *)
    notify;
  }

let server io : server =
  {
    request = On_request.handle;
    notify = On_notification.handle;
    client = client io;
  }

(* server stuff *)

let kind_of_packet =
  let k r = Result.get_ok r in
  
  let open Jsonrpc in function
    | Message ({id=Some id; method_=_; params=_} as r) ->
      `Request (id, Client_request.of_jsonrpc {r with id} |> k)
    | Message ({id=None; method_=_; params=_} as r) ->
      `Notification (Client_notification.of_jsonrpc {r with id=()} |> k)
    | Response _ -> failwith "response"

let run_server io server =
  (* send error with id or do smth with result *)
  let (|||) (id, r) on_ok = match r with
    | Ok v -> on_ok v
    | Error (e : JSonError.t) ->
      let e = Jsonrpc.Response.error id e in
      Lsp.Io.send io (Jsonrpc.Response e)
  in

  let process_request id r () =
    let reply = server.request server.client r in
    (id, reply) ||| (fun reply' ->
      let reply_json = Client_request.yojson_of_result r reply' in
      Lsp.Io.send io (Jsonrpc.Response (Jsonrpc.Response.ok id reply_json))
    )
  in

  let run_async f = Lwt.async (fun () -> f () |> Lwt.return) in

  let rec loop shutting_down =
    match Io.read io with
    | None -> loop shutting_down
    | Some p -> try match kind_of_packet p with
      | `Request (id, Client_request.E r) ->
        run_async (process_request id r);
        loop shutting_down
      | _ ->
        loop shutting_down
    with Invalid_argument _ ->
      let e = Jsonrpc.Response.error (`Int 0) {
        code = InternalError;
        message = "idk";
        data = None;
      } in
      Lsp.Io.send io (Jsonrpc.Response e)
      
  in loop false