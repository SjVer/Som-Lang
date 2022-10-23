open ANSITerminal

open Span
open Util

let r_single_line_span color span lines digits =
  (* print line before *)
  if span.start.line >= 2 then begin
    report_lineno digits (span.start.line - 1);
    prerr_endline (List.nth lines (span.start.line - 2));
  end;

  let line = List.nth lines (span.start.line - 1) in
  let part_before = String.sub line 0 (span.start.col - 1) in
  let part = String.sub line (span.start.col - 1) (span_length span) in
  let part_after_lenght = String.length line - span.end_.col + 1 in
  let part_after = String.sub line (span.end_.col - 1) part_after_lenght in
  
  (* print relevant line *)
  report_lineno digits (span.start.line);
  prerr_string [] part_before;
  prerr_string [Bold; Foreground color] part;
  prerr_string [] part_after;
  prerr_newline ();

  (* print marking *)
  report_marking digits line color span

let r_double_line_span _ _ _ _ = ignore

let r_multi_line_span _ _ _ _ = ignore

let r_span color span =
  let lines = read_lines span.file in
  let digits = String.length (string_of_int span.start.line) in

  (* print position *)
  prerr_char ' ';
  prerr_string cyan (show_span span);
  prerr_newline ();

  let report_fn = match kind_of_report span with
    | Single_line -> r_single_line_span
    | Double_line -> r_double_line_span
    | Multi_line -> r_multi_line_span
  in report_fn color span lines digits

let r_note note =
  prerr_string [Bold] " note: ";
  String.concat "\n       " (String.split_on_char '\n' note)
    |> prerr_endline

let r_span_and_notes color span notes =
  let print_tail = List.length notes > 0 in
  begin match span with
    | Some span -> r_span color span print_tail
    | None -> ()
  end;
  List.iter r_note notes
