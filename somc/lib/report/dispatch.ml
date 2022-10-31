open ANSITerminal

open Codes
open Error
open Span
open Util

let msg_and_color print = function
  | `Error e ->
    let (header, msg) = get_header_and_msg e in
    if print then begin
      prerr_string red header;
      Option.iter (fun c ->
        prerr_string red (f "[E%d]" c))
        (get_code_opt e)
    end;
    msg, Red
  | `Warning msg ->
    if print then prerr_string (bold Yellow) "warning";
    msg, Yellow
  | `Note msg ->
    if print then prerr_string (bold Cyan) "note";
    msg, Default

let r_single_line_span c s l d t m =
  (* print line before *)
  if s.start.line >= 2 then begin
    report_lineno d (s.start.line - 1);
    prerr_endline (List.nth l (s.start.line - 2));
  end;

  let line = List.nth l (s.start.line - 1) in
  let part_before = String.sub line 0 (s.start.col - 1) in
  let part = String.sub line (s.start.col - 1) (span_length s) in
  let part_after_lenght = String.length line - s.end_.col + 1 in
  let part_after = String.sub line (s.end_.col - 1) part_after_lenght in
  
  (* print relevant line *)
  report_lineno d (s.start.line);
  prerr_string [] part_before;
  prerr_string (bold c) part;
  prerr_string [] part_after;
  prerr_newline ();

  (* print marking *)
  report_marking d line c s t;
  Option.iter (prerr_string (bold c)) m;
  prerr_newline ()

let r_double_line_span _ _ _ _ _ = ignore

let r_multi_line_span _ _ _ _ _ = ignore

let r_span color print_tail txt span =
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
  in
  report_fn color span lines digits print_tail txt

let r_note note =
  prerr_string [Bold] " note: ";
  String.concat "\n       " (String.split_on_char '\n' note)
    |> prerr_endline

let rec r_related tail = function
  | [] -> ()
  | (kind, span) :: rs ->
    let tail' = tail || rs <> [] in
    let msg, color = msg_and_color false kind in
    r_span color tail' (Some msg) span;
    r_related tail rs