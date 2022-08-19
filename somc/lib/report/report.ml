module Error = Error
module Codes = Codes

open Error
open Codes
open Span
open ANSITerminal
open Util

let has_reported = ref false

let report_single_line_span span lines digits =
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
  prerr_string red part;
  prerr_string [] part_after;
  prerr_newline ();

  (* print marking *)
  report_marking digits line span

let report_double_line_span _ _ _ = ignore

let report_multi_line_span _ _ _ = ignore

let report_span span =
  let lines = read_lines span.file in
  let digits = String.length (string_of_int span.start.line) in

  (* print position *)
  prerr_char ' ';
  prerr_string cyan (show_span span);
  prerr_newline ();

  let report_fn = match kind_of_report span with
    | Single_line -> report_single_line_span
    | Double_line -> report_double_line_span
    | Multi_line -> report_multi_line_span
  in report_fn span lines digits

let report_note note =
  prerr_string [Bold] " note: ";
  String.concat "\n       " (String.split_on_char '\n' note)
    |> prerr_endline

let report error span notes =
  (* print leading newline if not the firs report *)
  if !has_reported then prerr_newline ()
  else has_reported := true;

  (* print "somekindof error[code]: msg" *)
  let (header, msg) = get_error_header_and_msg error in
  prerr_string red header;

  begin match error with Other_error _ -> () | _ ->
  prerr_string red (f "[E%03d]" (int_from_error error))
  end;

  prerr_string [Bold] (": " ^ msg);
  prerr_newline ();
  
  (* print span if given *)
  let print_tail = List.length notes > 0 in
  begin match span with
    | Some span -> report_span span print_tail
    | None -> ()
  end;

  (* print notes *)
  List.iter report_note notes