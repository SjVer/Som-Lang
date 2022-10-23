open Lsp
open Lsp.Types
open Somc
open Util

let publish_diagnostics server uri =
  Report.reports := [];
  let _ = Store.check server.T.store uri in
  
  let go (r: Report.t) =
    let sev, msg, code = match r.kind with
      | `Error e ->
        let (_, msg) = Report.Error.get_header_and_msg e in
        let code = Report.Codes.get_code_opt e in
        DiagnosticSeverity.Error, msg, code
      | `Warning msg -> DiagnosticSeverity.Warning, msg, None
      | `Note msg -> DiagnosticSeverity.Information, msg, None
    in
    let d = Diagnostic.create
      ~range:(range_from_span (Option.get r.span))
      ~severity:sev
      ~source:"som"
      ~message:msg
    in
    match code with
      | Some c -> d ~code:(`String (fmt "E%d" c)) ()
      | None -> d ()
  in

  let reports = List.filter
    (fun r -> match r.Report.span with
      | Some span -> span.Span.file = Uri.to_path uri
      | None -> false)
    !Report.reports
  in
  let diags = List.map go reports in
  
  let version = Text_document.version (Store.get_doc server.store uri) in
  server.client.notify (PublishDiagnostics {
    uri;
    version = Some version;
    diagnostics = diags;
  })