open Control
open Token
open Ast

(* =========================== toplevel =========================== *)

let rec prog f : ast =
  let ast' = ast f in
  consume f EOF "a toplevel statement" &
  ast'
  
and ast f =
  f := (fun f -> let t = toplevel f in t :: ast f)
    |= (fun f -> f & [])

and toplevel f : toplevel node =
  f := section

and section f : toplevel node =
  let nt = expect f (dummy `LOWERNAME) in
  let nn = mk nt (unpack_name nt.typ) in
  let ast = enclose f
    LBRACE "{"
    ast "section"
    RBRACE "}"
  in
  mk nt (TL_Section (nn, ast))
