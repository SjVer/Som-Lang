open Lsp.Types.Position
open Somc.Span

(* 
  1: 1 2 3 P 5 6 7

*)

let pos_in_span pos span =
  let ch = pos.character + 1 in

  if pos.line >= span.start.line && pos.line <= span.end_.line
  then begin
    if span.start.line = span.end_.line
    then ch >= span.start.col && ch <= span.end_.col
    else begin
      if pos.line = 
    end
  end else false

let find_tast_node tast pos =
  let open Somc.Typing.TAst in

  let find_in_toplevel {span; item; typ} =

  in

  let rec go = function
    | [] -> `Not_found
    | tl :: tls ->
      find_in_toplevel tl;
      go tls
  in go tast