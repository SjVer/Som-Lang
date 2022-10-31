open Report
open Report.Error
open Token

type frame =
  {
    mutable tokens: token list;
    mutable previous: token;
  }

exception Failed of bool
exception Error

(* error *)

let reported_spans : Span.t list ref = ref []

let can_report span =
  let exists = List.exists
    (fun s -> s = span)
    !reported_spans
  in
  if not exists then
    reported_spans := span :: !reported_spans;
  not exists

let error () = Stdlib.raise Error

let fail r = Stdlib.raise (Failed r)

let error_at s e n =
  if can_report s then
    make (`Error (Syntax_error e)) (Some s) n []
    |> report

let error_at_current f e n =
  error_at (List.hd f.tokens).span e n

(* tokens *)

let (&) a b = ignore a; b

let current f = List.hd f.tokens

let advance f =
  if (current f).typ <> EOF
  then begin
    f.previous <- current f;
    f.tokens <- List.tl f.tokens
  end;
  current f

let check f ts =
  List.exists ((tokens_eq) (current f).typ) ts

let consume f t str =
  if check f [t] then advance f
  else begin
    error_at_current f (Expected str) [];
    error ()
  end

let expect t f =
  if check f [t] then
    let t = current f in
    advance f & t
  else fail false

let matsch f ts =
  if check f ts then ((advance f) & true)
  else false

let at_end f = check f [EOF]

(* state *)

let backup f =
  {
    tokens = f.tokens;
    previous = f.previous;
  }

let restore srcf destf =
  destf.tokens <- srcf.tokens;
  destf.previous <- srcf.previous

(* flow *)

let skip f ts =
  if ts <> [] then begin
    let ts' = List.map
      (function `S t | `K t -> t)
      ts
    in
    while not (check f ts' || at_end f) do
      ignore (advance f)
    done;
    List.iter (function
      | `S t when check f [t] ->
        ignore (advance f)
      | _ -> ()
    ) ts
  end

let mk ?(g=false) i s =
  Ast.{
    span = {s with ghost = g};
    item = i;
  }

(* rules *)

(* [r1 +> r2] parsers [r1] followed by [r2] *)
let (+>) r1 r2 f = ignore (r1 f); r2 f

(* [r1 +< r2] parsers [r1] followed by [r2]
   but returning the value from [r1] *)
let (+<) r1 r2 f =
  let r = r1 f in
  ignore (r2 f);
  r

(* [(r +: v) f] returns [v (r f)] *)
let (+:) r v f = v (r f)

(* [(r += v) f] returns [v (r f) f] *)
let (+=) r v f = let r' = r f in v r' f

(* [r1 |= r2] attempts to parse [r1] and
   resorts to [r2] if [r1] failed. *)
let (|=) r1 r2 f =
  let f' = backup f in
  try r1 f
  with Failed _ ->
    restore f' f;
    r2 f

(* [r1 |> r2] attempts to parse [r1] and
   resorts to [r2] if [r1] failed, keeping
   the frame from [r1] *)
let (|>) r1 r2 f =
  try r1 f
  with Failed _ -> r2 f

(* [r |: v] attempts to parse [r] and
   returns [v] if [r] fails *)
let (|:) r v f =
  let f' = backup f in
  try r f
  with Failed _ ->
    restore f' f;
    v

(* [r |. (ts, v)] attempts to parse [r] and
   if it fails returns [v] if the next
   token is in [ts]. otherwise it fails *)
let (|.) r (ts, v) f =
  try r f
  with Failed b ->
    if check f ts then v
    else fail b

(* [r |! (str, ts)] attempts to parse [r] and
   reports [Expected str] if [r] failed. before
   propogation the failure it skips past [ts] *)
let (|!) r (str, ts) f =
  let f' = backup f in
  try r f
  with Failed r ->
    if not r then begin 
      let ps = (current f').span in
      let cs = (current f).span in
      let s = Span.concat_spans ps cs in
      if can_report s || can_report cs || can_report ps then
        error_at s (Expected str) [];
    end;
    skip f ts;
    fail true

(* parses [r] with frame [f] *)
let (:=) f r = r f

(* helpers *)

let enclose l lstr e _ r rstr f =
  let f' = backup f in
  let l' = expect l f in
  try
    let e' = e f in
    if not (matsch f [r]) then begin
      let s = (current f).span in
      if can_report s then
        (* let note = Printf.sprintf
          "try adding '%s' after the enclosed %s."
          rstr estr
        in *)
        (* error_at l'.span (Unclosed lstr) [note]; *)
        let e = Syntax_error (Expected (Util.f "'%s'" rstr)) in
        let r = make_error e (Some s) in
        let info = `Note (Util.f "unclosed '%s' here" lstr) in
        report (add_related info l'.span r);
    end;
    e'
  with Failed _ ->
    restore f' f;
    fail false

let cons r rs f =
  let r' = r f in
  r' :: rs f

let rec sl sep r f =
  f := cons r (sl sep (expect sep +> r))
    |: []

let snel sep r =
  cons r (sl sep r)

let ssntl sep r =
  cons (r +< expect sep) (snel sep r)

let rec many r =
  cons r (fun f ->
    let f' = backup f in
    try (many r) f
    with Failed _ ->
      restore f' f;
      []
  )