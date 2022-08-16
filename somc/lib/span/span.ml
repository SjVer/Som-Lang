open Lexing

type loc = {line: int; col: int; offset: int}

let show_loc p = string_of_int p.line ^ ":" ^ string_of_int p.col

type span = {file: string; start: loc; end_: loc}

(** [span_length span] returns 0 if [span] covers multiple lines *)
let span_length span =
  if span.start.line <> span.end_.line then 0
  else span.end_.col - span.start.col

let show_span s = 
  let f = if String.length s.file > 0 then s.file else "<file>" in
  if s.start.line = s.end_.line
  then if s.start.col + 1 >= s.end_.col
    then f ^ ":" ^ show_loc s.start
    else f ^ ":" ^ show_loc s.start ^ "-" ^ string_of_int s.end_.col
  else f ^ ":" ^ show_loc s.start ^ "-" ^ show_loc s.end_

let span_from_lexlocs (s, e) =
  let loc_to_loc l = {
    line = l.pos_lnum;
    col = l.pos_cnum - s.pos_bol + 1;
    offset = l.pos_bol + s.pos_cnum
  } in {
    file = s.pos_fname;
    start = loc_to_loc s;
    end_ = loc_to_loc e;
  }

let span_from_lexbuf lexbuf =
  span_from_lexlocs (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let offsets_from_span span = (span.start.offset, span.end_.offset)