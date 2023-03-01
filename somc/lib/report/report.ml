module Codes = Codes
module Error = Error
module Util = Util

open ANSITerminal

let enable_force_tty () = isatty := fun _ -> true

type kind =
  [ `Error of Error.t
  | `Warning of string
  | `Note of string
  ]

type t =
  {
    kind: kind;
    span: Span.t option;
    notes: string list;
    related: (kind * Span.t) list;
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

let add_note note r =
  {r with notes = r.notes @ [note]}
let add_related kind span r =
  {r with related = r.related @ [kind, span]}

(* functions *)

let reports : t list ref = ref []

let exit code =
  if !Config.in_lsp_mode then
    raise (Exit code)
  else exit code

let raise t = raise (Error t)

let report_compact r =
  let msg, color = Dispatch.msg_and_color false r.kind in
  let what = match r.kind with
    | `Error _ -> "error"
    | `Warning _ -> "warning"
    | `Note _ -> "note"
  in
  if Option.is_some r.span then begin
    prerr_string Util.cyan (Span.show_span (Option.get r.span));
    prerr_string [Bold] ": "
  end;

  prerr_string [Bold; Foreground color] what;
  prerr_string [Bold] (": " ^ msg);
  prerr_newline ()

let report_normal r =
  Util.maybe_newline ();
  let msg, color = Dispatch.msg_and_color true r.kind in
  let do_tail = r.related <> [] || r.notes <> [] in
  
  prerr_string [Bold] (": " ^ msg ^ "\n");
  Option.iter (Dispatch.r_span color do_tail None) r.span;

  Dispatch.r_related (r.notes <> []) r.related;
  List.iter Dispatch.r_note r.notes

let report r =
  if Option.is_some r.span then
    reports := !reports @ [r];

  if not !Config.in_lsp_mode then begin
    if !(Config.Cli.args).compact then report_compact r
    else report_normal r
  end