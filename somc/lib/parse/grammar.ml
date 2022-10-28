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
    try (many r) f
    with Failed _ ->
      restore f' f;
      []
  )

let withspan r f =
  let s = (current f).span in
  let r' = r f in
  let s' = Span.concat_spans
    s f.previous.span
  in
  r' s'

let dot f =
  ignore (consume f DOT "a terminating '.'")

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
    |= withspan definition

and section f s : toplevel node =
  let nt = expect (dummy `LOWERNAME) f in
  let nn = mk (unpack_name nt.typ) nt.span in
  let ast = enclose
    LBRACE "{"
    (ast [RBRACE]) "section"
    RBRACE "}"
    f
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
  
  let dir_segment f =
    let t = ident f in
    expect SLASH f &
    t
  in

  let rec finish_colon f =
    if matsch f [STAR] then
      mk IK_Glob f.previous.span
    else if check f [LBRACE] then
      withspan (fun f s ->
        let body' = body [] += (fun i _ -> mk i) in
        let imports = enclose
          LBRACE "{"
          (snel COMMA (withspan body')) "nested imports"
          RBRACE "}"
          f
        in
        mk (IK_Nested imports) s
      ) f
    else begin
      error_at_current f (Expected "'{' or '*'") [];
      mk IK_Error f.previous.span
    end
  and finish_name n f =
    (* let rn nn _ s = mk (IK_Rename (n, nn)) s in
    print_endline (show_token_typ (current f).typ);
    f := withspan (expect THICKARROW +> ident += rn)
      |: mk (IK_Simple n) n.span *)
    if matsch f [THICKARROW] then begin
      let s = f.previous.span in
      try
        let nn = ident f in
        mk (IK_Rename (n, nn)) s
      with Failed _ ->
        error_at_current f (Expected "an identifier") [];
        mk IK_Error s
    end
    else mk (IK_Simple n) n.span
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

  let i =
    f := many dir_segment += body
      |= body []
      |! ("an import path", [])
  in
  
  {
    span = s;
    item = TL_Import i;
  }

and definition f s : toplevel node =
  let b = binding expr f in
  let e = mk (TL_Definition b) s in
  dot f;
  e

(* =========================== binding =========================== *)

and binding efn f : value_binding =
  let p = withspan pattern f in
  let (t, e) = withspan (strict_binding efn) f in

  let e' = match t with
    | Some t -> {
        span = Span.{e.span with ghost = true};
        item = EX_Constraint (e, t);
      }
    | None -> e
  in
  {
    patt = p;
    expr = e';
  }

and strict_binding efn f s =
  let another_pattern f =
    let p = withspan pattern f in
    let (t, e) = withspan (strict_binding efn) f in

    let b =
      {
        patt = p;
        expr = e;
      }
    in
    t, mk (EX_Lambda b) s
  and body f =
    (* let t = if matsch f [COLON]
      then Some (typ f)
      else None
    in *)
    ignore (expect EQUAL f);
    None, efn f
  in
  f := body
    |= another_pattern
    |! ("a pattern or '='", [])

(* =========================== patterns =========================== *)

and pattern f s = simple_pattern f s

and simple_pattern f s =
  f := expect (dummy `LOWERNAME) +: (fun p ->
        mk (PA_Variable (unpack_name p.typ)) s)
    |= expect UNDERSCORE +: (fun n -> mk PA_Wildcard n.span)

(* =========================== expression ========================== *)

and expr f =
  f := withspan single_expr
    |! ("an expression", [])
    |: mk EX_Error f.previous.span

and single_expr f s : expr node =
  let mklit (n : token) =
    mk (EX_Literal (unpack_lit n.typ)) n.span
  in
  f := expect (dummy `INTEGER) +: mklit
    |= expect (dummy `FLOAT) +: mklit
    |= expect (dummy `CHARACTER) +: mklit
    |= expect (dummy `STRING) +: mklit
    |= expect EMPTYPARENS +: mklit

    |= (enclose LPAREN "(" expr "expression" RPAREN ")")
        +: (fun e -> mk (EX_Grouping e) s)
    |= (enclose LPAREN "(" expr "operator" RPAREN ")")
        +: (fun e -> mk (EX_Grouping e) s)

and infix_op f =
  let op t n = expect t +: (fun t -> mk n t.span) in
  f := op PLUS          "add"
    |= op MINUS         "sub"
    |= op STAR          "mul"
    |= op SLASH         "div"
    |= op CARET         "pow"
    |= op MODULO        "mod"
    |= op DBL_AMPERSAND "and"
    |= op DBL_CARET     "xor"
    |= op DBL_PIPE      "or" 
    |= op EQUAL         "eq" 
    |= op NOTEQUAL      "neq"
    |= op GREATER       "gr" 
    |= op GREATEREQUAL  "gre"
    |= op LESSER        "ls" 
    |= op LESSEREQUAL   "lse"