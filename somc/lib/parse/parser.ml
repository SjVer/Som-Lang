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

  let fail () = raise Failed

  let error_at span err notes =
    Report.make (`Error (Syntax_error err)) (Some span) notes []
    |> Report.report;
    fail ()

  let error_at_current p err notes =
    error_at (List.hd p.tokens).span err notes   

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
  if check t p then (advance p &> true)
  else false
let matschs ts p = List.exists (Fun.flip matsch p) ts

let consume t str p =
  if check t p then advance p
  else Handling.error_at_current p (Expected str) []

let at_end p = check EOF p
let not_at_end p = not (at_end p)