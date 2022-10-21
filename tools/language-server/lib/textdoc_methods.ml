open Lsp
open Lsp.Types
open Util

let hover (server: T.server) uri (pos: Types.Position.t) =
  let pos' = incr_pos pos in
  let node =
    Store.check server.store uri |>
    Find_node.find_tast_node pos' in
  let typ = Find_node.found_tast_node_type node in
  
  let contents = match typ with
    | Some typ' -> (`MarkedString MarkedString.{
        value = Somc.Typing.Types.show typ' false;
        language = Some "som";
      })
    | None -> `List []
  in
  let hover = match node with
    | `Not_found -> None 
    | node ->
      let range =
        Find_node.found_tast_node_span node
        |> Option.get
        |> Util.range_from_span
      in
      Some (Hover.create ~contents ~range ())
  in 
  Ok (hover)