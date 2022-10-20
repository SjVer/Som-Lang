open Lsp
open Lsp.Types

let fmt = Printf.sprintf

let internal_error_f =
  (fun msg -> Jsonrpc.Response.Error.{
    code = InternalError;
    message = msg;
    data = None;
  })

let markdown str =
  MarkupContent.create MarkupKind.Markdown str

let highlight str =
  MarkedString.{value = str; language = Some "som"}

let uri_from_docuri uri =
  Uri.t_of_yojson (DocumentUri.yojson_of_t uri)

let incr_pos (pos: Types.Position.t) =
  Types.Position.create (pos.line + 1) (pos.character + 1)