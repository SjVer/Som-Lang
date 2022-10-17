module Codes = Codes
module Error = Error
module Util = Util

open Error
open Codes
open Span
open ANSITerminal
open Util

let report_single_line_span color span lines digits =
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

let report_double_line_span _ _ _ _ = ignore

let report_multi_line_span _ _ _ _ = ignore

let report_span color span =
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
  in report_fn color span lines digits

let report_note note =
  prerr_string [Bold] " note: ";
  String.concat "\n       " (String.split_on_char '\n' note)
    |> prerr_endline

let report_span_and_notes span notes =
  let print_tail = List.length notes > 0 in
  begin match span with
    | Some span -> report_span Red span print_tail
    | None -> ()
  end;
  List.iter report_note notes

(* main functions *)

let report e =
  Util.maybe_newline ();

  (* print "somekindof error[code]: msg" *)
  let (header, msg) = get_error_header_and_msg e.error in
  prerr_string red header;

  begin match e.error with Other_error _ -> () | _ ->
  prerr_string red (f "[E%03d]" (int_from_error e.error))
  end;

  prerr_string [Bold] (": " ^ msg);
  prerr_newline ();
  report_span_and_notes e.span e.notes
  
let warning, note =
  let go color header msg span =
    if !(Config.Cli.args).mute then ()
    else begin
      Util.maybe_newline ();
      prerr_string [Bold; Foreground color] header;
      prerr_string [Bold] (": " ^ msg);
      prerr_newline ();
      
      (* print span if given *)
      match span with
        | Some span -> report_span color span false
        | None -> ()
  end
  in
  go Yellow "warning", go Cyan "note"