module Codes = Codes
module Error = Error
module Util = Util

open Error
open Codes
open ANSITerminal
open Util

type t =
  {
    kind:
      [ `Error of Error.t
      | `Warning of string
      | `Note of string ];
    span: Span.t option;
    notes: string list;
    related: t list;
  }

exception Exit of int
exception Error of t

(* constructors *)

let make kind span notes related =
  {kind; span; notes; related}
let make_simple kind span = make kind span [] []
let make_error e = make_simple (`Error e)
let make_warning msg = make_simple (`Warning msg)
let make_note msg = make_simple (`Note msg)

let add_note note report =
  {report with notes = report.notes @ [note]}
let add_related related report =
  {report with related = report.related @ [related]}

(* functions *)

let reports : t list ref = ref []

let exit code =
  if !Config.in_lsp_mode then
    raise (Exit code)
  else exit code

let raise t = raise (Error t)

let rec report r =
  if Option.is_some r.span then
    reports := !reports @ [r];

  if not !Config.in_lsp_mode then begin
    Util.maybe_newline ();

    let msg, color = match r.kind with
      | `Error e ->
        let (header, msg) = get_header_and_msg e in
        prerr_string red header;
        Option.iter (fun c ->
          prerr_string red (f "[E%d]" c))
          (get_code_opt e);
        msg, Red
      | `Warning msg ->
        prerr_string (bold Yellow) "warning";
        msg, Yellow
      | `Note msg ->
        prerr_string (bold Cyan) "note";
        msg, White
    in
    prerr_string [Bold] (": " ^ msg ^ "\n");
    Dispatch.r_span_and_notes color r.span r.notes;

    List.iter report r.related
  end