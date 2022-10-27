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

let error () = Stdlib.raise Error

let fail r = Stdlib.raise (Failed r)

let error_at t e n =
  make (`Error (Syntax_error e)) (Some t.span) n []
  |> report
  (* if tokens_eq t.typ EOF then error () *)

let error_at_current f e n =
  error_at (List.hd f.tokens) e n

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

let update srcf destf =
  destf.tokens <- srcf.tokens;
  destf.previous <- srcf.previous

(* flow *)

let skip f ts =
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

let mk i s =
  Ast.{
    span = s;
    item = i;
  }

let enclose f l lstr e estr r rstr =
  let l' = expect l f in
  let e' = e f in
  if not (matsch f [r]) then
    (error_at l' (Unclosed lstr)
      [Printf.sprintf
        "try adding '%s' after the enclosed %s."
        rstr estr];
      make_warning "here" (Some (current f).span) |> report)
  ;
  e'

(* rules *)

(* [r1 +> r2] parsers [r1] followed by [r2] *)
let (+>) r1 r2 f = r1 f & r2 f

(* [r1 +< r2] parsers [r1] followed by [r2]
   but returning the value from [r1] *)
let (+<) r1 r2 f = let r = r1 f in r2 f & r

(* [(r += v) f] returns [v (r f) f] *)
let (+=) r v f = let r' = r f in v r' f

(* [r1 |= r2] attempts to parse [r1] and
   resorts to [r2] if [r1] failed. *)
let (|=) r1 r2 f =
  let f' = backup f in
  try r1 f
  with Failed _ ->
    update f f';
    r2 f

(* [r1 |> r2] attempts to parse [r1] and
   resorts to [r2] if [r1] failed, keeping
   the frame from [r1] *)
let (|>) r1 r2 f =
  try r1 f
  with Failed _ -> r2 f

(* [r |: v] attempts to parse [r] and
   returns [v] if [r] fails *)
let (|:) r v = r |= (fun _  -> v)

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
    if not r then error_at_current f' (Expected str) [];
    skip f ts;
    fail true

(* parses [r] with frame [f] *)
let (:=) f r = r f