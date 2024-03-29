open Report.Error
open Token

let i = ignore
let (&>) a b = i a; b

type parser =
  {
    mutable tokens: token list;
    mutable previous: token;
  }

module Handling = struct

  exception Failed

  let had_error = ref false

  let fail () =
    had_error := true;
    raise Failed

  let error_at span err notes =
    Report.make (`Error (Syntax_error err)) (Some span) notes []
    |> Report.report;
    fail ()

  let error_at_current p err notes =
    error_at (List.hd p.tokens).span err notes

  let unclosed l lspan r rspan =
    Report.make_error (Syntax_error (Unclosed l)) (Some lspan)
    |> Report.add_related (`Error (Syntax_error (Expected r))) rspan
    |> Report.report;
    fail ()

end
      
let current p = List.hd p.tokens
let current_t p = (current p).typ

let advance p =
  if current_t p <> EOF then begin
    p.previous <- current p;
    p.tokens <- List.tl p.tokens;
  end;
  p.previous

let check t p = tokens_eq (current_t p) t
let checks ts p = List.exists (Fun.flip check p) ts

let check_peek t p =
  match p.tokens with
    | _ :: p :: _ -> p.typ = t
    | _ -> false

let matsch t p =
  if check t p then advance p &> true
  else false
let matschs ts p = List.exists (Fun.flip matsch p) ts

let consume t str p =
  if check t p then advance p
  else Handling.error_at_current p (Expected str) []

let at_end p = check EOF p
let not_at_end p = not (at_end p)

exception Backtrack

let try_parse p parsefn =
  let old_tokens = p.tokens in
  let old_previous = p.previous in
  try parsefn p
  with Backtrack as e ->
    p.tokens <- old_tokens;
    p.previous <- old_previous;
    raise e

let backtrack () = raise Backtrack

let skip_until ts p =
  while not (checks (EOF :: ts) p) do
    i (advance p)
  done