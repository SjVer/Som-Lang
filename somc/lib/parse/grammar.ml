open Report.Error
open Control
open Token
open Ast

(* =========================== helpers ============================ *)

let cons r rs f =
  let r' = r f in
  r' :: rs f

let snel sep x =
  let rec tail f =
    f := expect sep +> (cons x tail)
      |: []
  in
  cons x tail

let rec many r =
  cons r (fun f ->
    let f' = backup f in
    try begin
      let r' = (many r) f' in
      update f' f;
      r'
    end
    with Failed _ -> []
  )

let withspan r f =
  let s = (current f).span in
  let r' = r f in
  let s' = Span.concat_spans
    s f.previous.span
  in
  r' s'

(* =========================== toplevel =========================== *)

let rec prog f : ast =
  let ast' = ast [EOF] f in
  consume f EOF "EOF" &
  ast'
  
and ast ts f : ast =
  f := (cons toplevel (ast ts))
    |. (ts, [])
    |! ("expected a toplevel statement", [`S DOT])
    |> ast ts

and toplevel f : toplevel node =
  f := withspan section
    |= withspan import

and section f s : toplevel node =
  let nt = expect (dummy `LOWERNAME) f in
  let nn = mk (unpack_name nt.typ) nt.span in
  let ast = enclose f
    LBRACE "{"
    (ast [RBRACE]) "section"
    RBRACE "}"
  in
  mk (TL_Section (nn, ast)) s

and import f s : toplevel node =
  ignore (expect HASH f); 

  let ident f =
    let i =
      f := expect (dummy `LOWERNAME)
        |= expect (dummy `UPPERNAME)
    in
    mk (unpack_name i.typ) i.span
  in
  
  let dir =
    let dir_segment f =
      let t = ident f in
      expect SLASH f &
      t
    in
    f := many dir_segment
      |! ("an import path", [])
  in

  let rec finish_colon f =
    if matsch f [STAR] then
      mk IK_Glob f.previous.span
    else if check f [LBRACE] then
      withspan (fun f s ->
        let body' = body [] += (fun i _ -> mk i) in
        let imports = enclose f
          LBRACE "{"
          (snel COMMA (withspan body')) "nested imports"
          RBRACE "}"
        in
        mk (IK_Nested imports) s
      ) f
    else begin
      error_at_current f (Expected "'{' or '*'") [];
      mk IK_Error f.previous.span
    end
  and finish_name n f =
    let rn nn _ s = mk (IK_Rename (n, nn)) s in
    f := withspan (expect THICKARROW +> ident += rn)
      |: mk (IK_Simple n) n.span
  and body dir f =
    let path = many (ident +< expect DBL_COLON) f in
    let kind =
      f := ident += finish_name
        |= finish_colon
    in
    {
      dir;
      path;
      kind;
    }
  in
  
  {
    span = s;
    item = TL_Import (body dir f);
  }