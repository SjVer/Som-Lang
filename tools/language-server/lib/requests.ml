open Lsp.Types
open Util
open Somc

let semantic_tokens server uri =
  let ast = Store.parse server.T.store uri in
  if ast = [] then Ok (None)
  else begin
    let tokens = Semantics.get_tokens ast uri in
    let data = Semantics.encode_tokens tokens in
    Ok (Some (SemanticTokens.create ~data ()))
  end

let hover server uri pos =
  let pos' = incr_pos pos in
  let node =
    Store.check server.T.store uri |>
    Find_node.find_tast_node pos'
  in
  let typ =
    Find_node.found_tast_node_type node |>
    Option.map (fun t -> Typing.Types.show t false)
  in
  let doc = None (* TODO: find docstring *) in

  let contents =
    let mk s = `MarkupContent MarkupContent.{
        value = s;
        kind = MarkupKind.Markdown;
      }
    in match typ, doc with
      | Some typ, None ->
        mk (fmt "```som\n%s\n```" typ)
      | Some typ, Some doc ->
        mk (fmt "```som\n%s\n```\n---\n%s" typ doc)
      | None, Some doc -> mk doc
      | None, None -> `List []
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