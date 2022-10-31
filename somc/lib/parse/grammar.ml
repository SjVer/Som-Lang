open Report.Error
open Control
open Token
open Ast

(* =========================== helpers ============================ *)

(* if true produce stuff like EX_Grouping *)
let groups = false

let ws r f =
  let s = (current f).span in
  let item = r f in
  let s' = Span.concat_spans
    s f.previous.span
  in
  mk item s'

let dot f =
  ignore (
    f := expect DOT
      |! ("a terminating '.'", [`S DOT])
  )

let left_assoc f efn sepfn mkfn =
  let rec go lhs =
    try
      let sep = sepfn f in
      let rhs = efn f in
      let s = Span.concat_spans lhs.span rhs.span in
      go (mkfn lhs sep rhs s)
    with Failed _ ->
      lhs
  in go (efn f)

(* =========================== toplevel =========================== *)

let rec prog f : ast =
  let ast' = ast [EOF] f in
  consume f EOF "EOF" &
  ast'
  
and ast ts f : ast =
  f := (cons (ws toplevel) (ast ts))
    |. (ts, [])
    |! ("expected a toplevel statement", [`S DOT])
    |> ast ts

and toplevel f : toplevel =
  f := section
    |= import
    |= definition

and section f : toplevel =
  let nt = expect (dummy `LOWERNAME) f in
  let nn = mk (unpack_name nt.typ) nt.span in
  let ast = enclose
    LBRACE "{"
    (ast [RBRACE]) "section"
    RBRACE "}"
    f
  in
  TL_Section (nn, ast)

and import f : toplevel =
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
      ws (fun f ->
        let body' = body [] in
        let imports = enclose
          LBRACE "{"
          (snel COMMA (ws body')) "nested imports"
          RBRACE "}"
          f
        in
        IK_Nested imports
      ) f
    else begin
      error_at_current f (Expected "'{' or '*'") [];
      mk IK_Error f.previous.span
    end
  and finish_name n f =
    (* let rn nn _ s = mk (IK_Rename (n, nn)) s in
    print_endline (show_token_typ (current f).typ);
    f := ws (expect THICKARROW +> ident += rn)
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
  TL_Import i

and definition f : toplevel =
  let b = binding true EQUAL "=" (ws expr) f in
  dot f;
  TL_Definition b

(* =========================== binding =========================== *)

and binding must sep sepstr efn f : value_binding =
  let p = ws pattern f in
  let (t, e) = strict_binding must sep sepstr efn f in

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

and strict_binding must sep sepstr efn f =
  let another_pattern f =
    let patt = ws pattern f in
    let (t, expr) = strict_binding must sep sepstr efn f in
    let b = {patt; expr} in
    let s = Span.concat_spans patt.span expr.span in
    t, mk ~g:true (EX_Lambda b) s
  in
  let body f =
    (* let t = if matsch f [COLON]
      then Some (typ f)
      else None
    in *)
    ignore (expect sep f);
    let e =
      f := efn
        |: mk ~g:true EX_Error (current f).span
    in
    None, e
  in
  if must then
    f := body
      |= another_pattern
      |! (Printf.sprintf "a pattern or '%s'" sepstr, [])
  else
    f := body
      |= another_pattern

(* =========================== patterns =========================== *)

and pattern f : pattern = simple_pattern f

and simple_pattern f : pattern =
  f := expect (dummy `LOWERNAME) +: (fun p -> PA_Variable (unpack_name p.typ))
    |= expect UNDERSCORE +: (fun _ -> PA_Wildcard)

(* =========================== expression ========================= *)

and expr f : expr =
  let bind b f =
    ignore (expect THICKARROW f);
    let e = ws expr f in
    EX_Binding (b, e)
  in
  f := binding false EQUAL "=" (ws lambda_expr) += bind
    |= lambda_expr
    |! ("an expression", [])
    |: EX_Error

and lambda_expr f : expr =
  let lambda f =
    try
      let b = binding true ARROW "->" (ws lambda_expr) f in
      EX_Lambda b
    with Failed _ ->
      (* (* TODO: we're skipping the token that was supposed
         to be '->' and ignoring an expression so that no
         duplicate errors are reported. might be shit. *)
      ignore (advance f);
      seq_expr f & *)
      EX_Error
  in
  f := expect BACKSLASH +> lambda
    |= seq_expr

and seq_expr f : expr =
  let e =
    left_assoc f
      (ws tuple_expr)
      (expect COMMA)
      (fun e1 _ e2 s -> mk (EX_Sequence (e1, e2) ) s)
  in e.item

and tuple_expr f : expr = 
  f := ssntl SEMICOLON (ws constr_expr) +: (fun es -> EX_Tuple es)
    |= constr_expr

and constr_expr f : expr =
  f := infix_expr 7

and infix_expr prec f : expr =
  if prec >= 0 then
    (* TODO: associativity? *)
    let e = left_assoc f
      (ws (infix_expr (prec - 1)))
      (ws (infix_op prec))
      (fun lhs op rhs s -> mk (EX_Application (op, [lhs; rhs])) s)
    in
    e.item
  else base_expr f

and base_expr f : expr =
  let constr c f =
    let es = many (ws single_expr) f in
    EX_Construct (c, es)
  in
  let app e f =
    let es = many (ws single_expr) f in
    EX_Application (e, es)
  in
  let unary o f =
    let e = ws base_expr f in
    EX_Application (o, [e])
  in
  f := upper_longident += constr
    |= ws single_expr += app
    |= single_expr
    |= ws unary_op += unary
    |! ("an expression", [])
    |: EX_Error

and single_expr f : expr =
  let mklit (n : token) =
    EX_Literal (unpack_lit n.typ)
  in
  let finish_external f =
    try
      let i = expect (dummy `LOWERNAME) f in
      EX_External (unpack_name i.typ)
    with Failed _ ->
      error_at_current f (Expected "an identifier") [];
      EX_Error
  in
  f := expect (dummy `INTEGER) +: mklit
    |= expect (dummy `FLOAT) +: mklit
    |= expect (dummy `CHARACTER) +: mklit
    |= expect (dummy `STRING) +: mklit
    |= expect EMPTYPARENS +: mklit

    |= lower_longident +: (fun i -> EX_Identifier i)
    |= expect HASH +> finish_external

    |= enclose LBRACKET "[" list "list" RBRACKET "]"
        += ghost_list
    
    (* |= enclose LPAREN "(" any_op "operator" RPAREN ")" *)
    |= enclose LPAREN "(" (ws expr) "expression" RPAREN ")"
        +: (fun e -> if groups then EX_Grouping e else e.item)

(* ====================== expression helpers ===================== *)

and infix_ops =
  [
    [CARET, "^"];
    [STAR, "*"; SLASH, "/"; MODULO, "%"];
    [PLUS, "+"; MINUS, "-"];
    [GREATER, ">"; GREATEREQUAL, ">="; LESSER, "<"; LESSEREQUAL, "<="];
    [EQUAL, "="; NOTEQUAL, "/="];
    [DBL_AMPERSAND, "&&"];
    [DBL_CARET, "^^"];
    [DBL_PIPE, "||"]; 
  ]

and unary_ops =
  [
    PLUS, "~+";
    MINUS, "~-";
    BANG, "~!";
  ]

and operator f = function
  | [] -> fail false
  | (tt, on) :: tl ->
    let f' = backup f in
    try
      let t = expect tt f in
      let i = Ident.Ident on in
      EX_Identifier (mk i t.span)
    with Failed _ ->
      restore f' f;
      operator f tl

and infix_op prec f : expr =
  operator f (List.nth infix_ops prec)

and unary_op f : expr =
  operator f unary_ops

and list f =
  if check f [RBRACKET] then []
  else begin
    let e = ws tuple_expr f in
    if matsch f [COMMA] then
      e :: list f
    else [e]
  end

and ghost_list es f =
  let span nodes =
    Span.concat_spans
      (List.hd nodes).span
      (List.hd (List.rev nodes)).span
  in
  let rec go = function
    | [] ->
      let nil = mk ~g:true (Ident.Ident "[]") f.previous.span in
      mk ~g:true (EX_Construct (nil, [])) nil.span
    | e :: es ->
      let cons = mk ~g:true (Ident.Ident "::") e.span in
      let tail = go es in
      let s = span (e :: es) in
      mk ~g:true (EX_Construct (cons, [e; tail])) s
  in (go es).item
 
(* ========================== identifiers ========================= *)

and longident_path f =
  let ident f =
    let i = expect (dummy `LOWERNAME) f in
    unpack_name i.typ
  in
  f := many (ident +< expect DBL_COLON)
    |: []

and lower_longident f =
  let go f =
    let p = longident_path f in
    let i = expect (dummy `LOWERNAME) f in
    Ident.from_list (p @ [unpack_name i.typ])
  in
  ws go f

and upper_longident f =
  let go f =
    let p = longident_path f in
    let i = expect (dummy `UPPERNAME) f in
    Ident.from_list (p @ [unpack_name i.typ])
  in
  ws go f

