open Control
open Token
open Ast

(* =========================== toplevel =========================== *)

let rec prog f : ast =
  let ast' = ast [EOF] f in
  consume f EOF "EOF" &
  ast'
  
and ast ts f : ast =
  f := (fun f -> let t = toplevel f in t :: ast ts f)
    |. (ts, [])
    |! ("expected a toplevel statement", [`S DOT])
    |> (fun f -> print_endline "try"; let a = ast ts f in print_endline "done"; a)

and toplevel f : toplevel node =
  f := section
    |= import

and section f : toplevel node =
  let nt = expect (dummy `LOWERNAME) f in
  let nn = mk nt (unpack_name nt.typ) in
  let ast = enclose f
    LBRACE "{"
    (ast [RBRACE]) "section"
    RBRACE "}"
  in
  mk nt (TL_Section (nn, ast))

and import f : toplevel node =
  ignore (expect HASH f); 

  let dir_segment f =
    let s f =
      f := (expect (dummy `LOWERNAME))
        |= (expect (dummy `UPPERNAME))
    in
    let t = s f in
    expect SLASH f &
    t
  in
  
  let rec body f =
    f := dir_segment +> body
      |! ("expected a path", [])
  in
  body f