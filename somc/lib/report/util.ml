open Span
open ANSITerminal

let f = Printf.sprintf

let read_lines name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  (loop []) @ [""]

type report_kind = Single_line | Double_line | Multi_line

let kind_of_report span =
  if span.start.line = span.end_.line then Single_line
  else if span.start.line + 1 = span.end_.line then Double_line
  else Multi_line

let cyan = [Bold; Foreground Cyan]
let red = [Bold; Foreground Red]

let report_lineno digits line =
  (* print " lineno | " *)
  prerr_string cyan (f " %*d │ " digits line)

let report_marking digits line span =
  prerr_string cyan (String.make digits ' ' ^ "  ╵ ");

  (* padding before "^~~~~" *)
  let ln_before = String.sub line 0 (span.start.col - 1) in
  for c = 0 to String.length ln_before - 1 do
    prerr_char (if ln_before.[c] = '\t' then '\t' else ' ')
  done;

  (* actual marking *)
  prerr_string red "^";
  if span_length span >= 2
  then prerr_string red (String.make (span_length span - 1) '~');
  prerr_newline ()
