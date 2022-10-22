open Lsp
open Lsp.Types
open Somc
open Util

let publish_diagnostics server uri =
  Report.diagnostics := [];
  let _ = Store.check server.T.store uri in
  
  let go (span, sev, msg, code) =
    let sev' = match sev with
      | `Error -> DiagnosticSeverity.Error
      | `Warning -> DiagnosticSeverity.Warning
      | `Note -> DiagnosticSeverity.Information
    in
    let d = Diagnostic.create
      ~range:(range_from_span span)
      ~severity:sev'
      ~source:"som"
      ~message:msg
    in
    match code with
      | Some c -> d ~code:(`String (fmt "E%d" c)) ()
      | None -> d ()
  in
  let diags = List.map go !Report.diagnostics in

  server.client.notify (PublishDiagnostics {
    uri;
    version = None;
    diagnostics = diags;
  })