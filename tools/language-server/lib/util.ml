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
  MarkupContent.create ~kind:MarkupKind.Markdown ~value:str

let highlight str =
  MarkedString.{value = str; language = Some "som"}

let incr_pos (pos: Position.t) =
  Position.create
    ~line:(pos.line + 1)
    ~character:(pos.character + 1)

let range_from_span (span: Somc.Span.t) =
  let start = Position.create
    ~line:(span.start.line - 1)
    ~character:(span.start.col - 1)
  and end_ = Position.create
    ~line:(span.end_.line - 1)
    ~character:(span.end_.col - 1)
  in
  Range.create ~start ~end_