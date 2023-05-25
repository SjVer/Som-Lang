module Codes = Codes
module Error = Error
module Util = Util

open ANSITerminal

let has_reported = ref false
let has_errored = ref false

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

exception Exit
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

let exit () = raise Exit

let raise t = raise (Error t)

let last_was_compact = ref false

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
  let isnt_compact = r.related <> [] || r.notes <> [] in
  if isnt_compact || not !last_was_compact then
    Util.maybe_newline ();
  last_was_compact := not isnt_compact;
  
  let msg, color = Dispatch.msg_and_color true r.kind in
  prerr_string [Bold] (": " ^ msg ^ "\n");
  Option.iter (Dispatch.r_span color isnt_compact None) r.span;

  Dispatch.r_related (r.notes <> []) r.related;
  List.iter Dispatch.r_note r.notes

let report r =
  has_reported := true;
  begin match r.kind with
    | `Error _ -> has_errored := true
    | _ -> ()
  end;

  if Option.is_some r.span then
    reports := !reports @ [r];

  if not !Configs.in_lsp_mode then begin
    if !Configs.Cli.args.compact then report_compact r
    else report_normal r;
  end;

  flush_all ()

let report_note msg =
  make_note msg None
  |> report
