open Span
open ANSITerminal

let has_reported = ref false
let maybe_newline () =
  if !has_reported then prerr_newline ()
  else has_reported := true

let f = Printf.sprintf

(* to be set to ReadFile.call *)
let read_file_fn = ref (fun (""|_) -> assert false)

let read_lines f =
  let source = !read_file_fn f in
  String.split_on_char '\n' source

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

let report_marking digits line color span print_tail =
  let tail = if print_tail then "  │ " else "  ╵ " in
  prerr_string cyan (String.make digits ' ' ^ tail);

  (* padding before "^~~~~" *)
  let ln_before = String.sub line 0 (span.start.col - 1) in
  for c = 0 to String.length ln_before - 1 do
    prerr_char (if ln_before.[c] = '\t' then '\t' else ' ')
  done;

  (* actual marking *)
  let style = [Bold; Foreground color] in
  prerr_string style "^";
  if span_length span >= 2
  then prerr_string style (String.make (span_length span - 1) '~');
  prerr_newline ()
