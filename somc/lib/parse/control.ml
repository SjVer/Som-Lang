open Report
open Report.Error
open Token

type frame =
  {
    mutable tokens: token list;
    mutable previous: token;
  }

exception Failed
exception Error

(* error *)

let error_at t e n =
  make (`Error (Syntax_error e)) (Some t.span) n []
  |> report

let error_at_current f =
  error_at (List.hd f.tokens)

(* tokens *)

let (&) a b = ignore a; b

let current f =
  List.hd f.tokens

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
    Stdlib.raise Error
  end

let expect f t =
  if check f [t] then
    let t = current f in
    advance f & t
  else Stdlib.raise Failed

let matsch f ts =
  if check f ts then ((advance f) & true)
  else false

let at_end f =
  check f [EOF]

(* state *)

let _push f =
  {
    tokens = f.tokens;
    previous = f.previous;
  }

(* flow *)

let skip_to f ts =
  while not (check f ts) do
    ignore (advance f)
  done

let mk t i =
  Ast.{
    span = t.Token.span;
    item = i;
  }

let enclose f l lstr e estr r rstr =
  let l' = expect f l in
  let e' = e f in
  if not (matsch f [r]) then
    error_at l' (Unclosed lstr)
      [Printf.sprintf
        "try adding '%s' after the enclosed %s."
        rstr estr];
  e'

let (|=) r1 r2 = fun f ->
  try r1 f
  with Failed -> r2 f

let (|!) r1 str = fun f ->
  try r1 f
  with Failed ->
    error_at_current f (Expected str) [];
    Stdlib.raise Error

let (:=) f r = r f

let (~::) a = fun b -> a :: b