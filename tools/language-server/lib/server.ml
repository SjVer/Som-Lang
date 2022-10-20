open Lsp
module JSonError = Jsonrpc.Response.Error

(* log stuff *)

module Log = (val Logs.src_log (Logs.Src.create "som-lsp"))

let setup_log () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);
  Log.info (fun f -> f "Initialized logging")

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

let make io : server =
  let client =
    let notify n =
      let n' = Server_notification.to_jsonrpc n in
      Io.send io (Jsonrpc.Message {n' with id = None})
    in
    {
      notify;
    }
  in
  {
    request = On_request.handle;
    notify = On_notification.handle;
    client;
  }

(* server stuff *)

let kind_of_packet =
  let k r = Result.get_ok r in
  
  let open Jsonrpc in function
    | Message {id=None; method_="exit"; params=_} ->
      `Exit
    | Message ({id=None; method_=_; params=_} as r) ->
      `Notification (Client_notification.of_jsonrpc {r with id=()} |> k)
    | Message ({id=Some id; method_=_; params=_} as r) ->
      `Request (id, Client_request.of_jsonrpc {r with id} |> k)
    | Response _ -> failwith "response"

let run_async f = Lwt.async (fun () -> f () |> Lwt.return)

let run io server =
  (* send error with id or do smth with result *)
  let (|||) (id, r) on_ok = match r with
    | Ok v -> on_ok v
    | Error (e : JSonError.t) ->
      let e = Jsonrpc.Response.error id e in
      Io.send io (Jsonrpc.Response e)
  in

  let rec loop () =
    try match Io.read io with
      | None ->
        Log.warn (fun f -> f "No packet");
        loop ()
      | Some p ->
        match kind_of_packet p with
        | `Exit -> Log.info (fun f -> f "Received exit")
        | `Notification n ->
          Log.debug (fun f -> f "Received notification");
          begin fun () ->
            server.notify server.client n
          end |> run_async |> loop
        | `Request (id, Client_request.E r) ->
          Log.debug (fun f -> f "Received request");
          begin fun () ->
            let reply = server.request server.client r in
            (id, reply) ||| (fun reply' ->
              Log.info (fun f -> f "Sending reply");
              let reply_json = Client_request.yojson_of_result r reply' in
              Io.send io (Jsonrpc.Response (Jsonrpc.Response.ok id reply_json))
            )
          end |> run_async |> loop
    with _ ->
      (* if [kind_of_packet] failed *)
      let e = Jsonrpc.Response.error (`String "") {
          code = InternalError;
          message = "Invalid packet";
          data = None;
        }
      in Io.send io (Jsonrpc.Response e) |> loop
      
  in loop ()