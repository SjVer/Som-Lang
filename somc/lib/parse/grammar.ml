open Report.Error
open Parser
open Handling
open Token
open Ast

(* =========================== helpers ============================ *)

(* if true produce stuff like EXGrouping *)
let groups = false

let mk span item = {span; item}
let mk_g span item = mk {span with ghost = true} item
let mk_t item (t: token) = {span = t.span; item}

let catspans s1 s2 = Span.concat_spans s1 s2
let catnspans n1 n2 = Span.concat_spans n1.span n2.span

let left_assoc (p : parser) itemfn sep makefn =
  let rec go lhs =
    if matsch sep p then
      let rhs = itemfn p in
      let s = catnspans lhs rhs in
      go (makefn lhs rhs s)
    else
      lhs
  in go (itemfn p)

let many (p : parser) checkfn parsefn =
  let rec go acc =
    if checkfn p then acc @ [parsefn p] |> go
    else acc
  in go []

let many_try p parsefn =
  let rec go acc =
    try try_parse p (fun p -> acc @ [parsefn p] |> go)
    with Backtrack -> acc
  in go []

let try_and_skip_until p parsefn until default =
  try parsefn p
  with Failed ->
    skip_until until p;
    mk p.previous.span default

let consume_ident p =
  if matsch (dummy `LOWERNAME) p then
    mk_t (unpack_str p.previous.typ) p.previous
  else
    try error_at_current p (Expected "an identifier") []
    with Failed ->
      let t = mk_t "<parse error>" p.previous in
      i (advance p) &> t

(* =========================== toplevel =========================== *)

let rec parse_file p : ast =
  let toplevel' p =
    try Some (toplevel p)
    with Failed ->
      skip_until [LET; TYPE; EXT; MOD; USE; FROM] p;
      None
  in
  let tls = many p not_at_end toplevel' in
  consume EOF "end of file" p &>
  List.map Option.get (List.filter Option.is_some tls)

and toplevel p : toplevel node =
  let start_s = (current p).span in
  let tl = match current_t p with
    | MOD -> toplevel_module p
    | USE -> toplevel_import_normal p
    | FROM -> toplevel_import_from p
    | LET -> toplevel_value_definition p
    | TYPE -> toplevel_type_definition p
    | EXT -> toplevel_extern_definition p
    | _ -> error_at_current p (Expected "a toplevel statement") []
  in
  let s = catspans start_s p.previous.span in
  mk s tl

(* TODO: `use ... as ...` *)

and toplevel_module p =
  i (advance p);
  let name = consume_ident p in
  i (consume LBRACE "\"{\"" p);
  let tls = many p (fun p -> not (check RBRACE p)) toplevel in
  i (consume RBRACE "\"}\"" p);
  TLModule (name, tls)

and toplevel_import_normal p =
  i (advance p);
  let i_path = finish_import_path p in
  TLImport { i_path; i_kind = mk_t IK_Module p.previous }

and toplevel_import_from p =
  i (advance p);
  let i_path = finish_import_path p in
  i (consume USE "\"use\"" p);
  let start_s = (current p).span in
  let kind =
    (* `use *` or `use ...` *)
    if matsch STAR p then IK_Glob
    else finish_import_from p
  in
  let s = catspans start_s p.previous.span in
  TLImport { i_path; i_kind = mk s kind}

and toplevel_value_definition p =
  i (advance p);
  let name = consume_ident p in
  let vd_name = {name with item = Ident.Ident name.item} in
  
  (* expression *)
  let (t, e) = strict_binding p expression EQUAL "=" in
  TLValueDef {vd_name; vd_expr = wrap_type_constraint t e}

and toplevel_type_definition p =
  i (advance p);

  (* params *)
  let parsefn p = mk_t (unpack_str p.previous.typ) p.previous in
  let params = many p (matsch (dummy `PRIMENAME)) parsefn in

  (* name *)
  let name =
    if matsch (dummy `UPPERNAME) p then
      mk_t (unpack_str p.previous.typ) p.previous
    else
      try error_at_current p (Expected "an identifier") []
      with Failed ->
        let t = mk_t "<parse error>" p.previous in
        i (advance p) &> t
  in
  let td_name = {name with item = Ident.Ident name.item} in

  (* type *)
  let typ =
    if matsch IS p then typ p
    else begin
      i (consume OF "\"is\" or \"of\"" p);
      failwith "TODO: parse complex type definition"
    end
  in

  (* wrap type in TYForall if there's parameters *)
  let td_type =
    if params = [] then typ
    else mk_g typ.span (TYForall (params, typ))
  in
  TLTypeDef {td_name; td_type}

and toplevel_extern_definition p =
  i (advance p);

  let ed_native_name = consume_ident p in

  (* different name is optional *)
  let ed_name =
    let f i = nmap i (fun i -> Ident.Ident i) in 
    if matsch AS p then f (consume_ident p)
    else f ed_native_name
  in

  (* we don't do anything with the arguments *)
  while not (check COLON p) do
    try_and_skip_until p pattern [COLON] PAWildcard
    |> ignore
  done;
  
  (* type annotation is not optional *)
  i (consume COLON "\":\"" p);
  let ed_type = typ p in

  TLExternDef {ed_native_name; ed_name; ed_type}

(* ============================ imports =========================== *)

and finish_import_from p =
  let go p =
    (* single "sub-import" *)
    let path = longident_path p in
    let mk_t' p i = mk_t i p.previous in
    let ident =
      if matschs [dummy `LOWERNAME; dummy `UPPERNAME] p then
        mk_t' p (unpack_str p.previous.typ)
      else error_at_current p (Expected "an indentifier") []
    in
    mk_t' p (IK_Simple (path @ [ident]))
  in
  (* at least one nested import, maybe more *)
  let first = go p in
  let other = many p (matsch COMMA) go in
  IK_Nested (first :: other)

and finish_import_path p =
  let path = longident_path p in
  let last = consume (dummy `LOWERNAME) "an identifier" p in
  path @ [mk_t (unpack_str last.typ) last]

(* ============================ binding =========================== *)

and binding (p : parser) exprfn sep sepstr : value_binding =
  let vb_patt = try_and_skip_until p pattern [sep; COLON] PAWildcard in
  let (t, e) = strict_binding p exprfn sep sepstr in
  {vb_patt; vb_expr = wrap_type_constraint t e}

and strict_binding p exprfn sep sepstr =
  if checks [COLON; sep] p then begin
    (* type annotation *)
    let t = if matsch COLON p
      then Some (typ p)
      else None
    in
    let pat_or_ty = if t = None then "pattern" else "type" in
    (* sep and body *)
    i (consume sep ("a " ^ pat_or_ty ^ " or \"" ^ sepstr ^ "\"") p);
    let e = exprfn p in
    t, e
  end else
    (* another pattern *)
    let vb_patt = try_and_skip_until p pattern [sep; COLON] PAWildcard in
    let (t, vb_expr) = strict_binding p exprfn sep sepstr in
    let s = catnspans vb_patt vb_expr in
    t, mk_g s (EXLambda {vb_patt; vb_expr})

and wrap_type_constraint t e =
  match t with
    | Some t -> {
        span = {e.span with ghost = true};
        item = EXConstraint (e, t);
      }
    | None -> e

(* ============================ patterns ========================== *)

and pattern p : pattern node =
  (* TODO *)
  atom_pattern p

and atom_pattern p : pattern node =
  let curr = current p in
  let mk' i = mk curr.span i in
  
  if matsch (dummy `LOWERNAME) p then mk' (PAVariable (unpack_str curr.typ))
  else if matsch UNDERSCORE p then mk' PAWildcard
  else error_at_current p (Expected "a pattern") []

(* =========================== expression ========================= *)

and expression p : expr node = let_expression p

and let_expression p : expr node =
  if matsch LET p then begin
    let let_s = p.previous.span in
    let bind = binding p let_expression EQUAL "=" in
    i (consume IN "\"in\"" p);
    let body = let_expression p in
    let s = catspans let_s body.span in 
    mk s (EXBinding (bind, body))
  end
  else sequence_expression p

and sequence_expression p : expr node =
  let mk_seq e1 e2 s = mk s (EXSequence (e1, e2)) in
  left_assoc p lambda_expression COMMA mk_seq

and lambda_expression p : expr node =
  if matsch BACKSLASH p then begin
    let start_s = p.previous.span in
    let bind = binding p tuple_expression ARROW "->" in
    let s = catspans start_s bind.vb_expr.span in
    mk s (EXLambda bind)
  end else tuple_expression p

and tuple_expression p : expr node =
  let e = t_constr_expression p in
  let es = many p (matsch SEMICOLON) t_constr_expression in

  if es <> [] then
    let s = catnspans e (List.hd es) in
    mk s (EXTuple (e :: es))
  else e

and t_constr_expression p : expr node =
  let e = infix_expression p max_precedence in
  if matsch COLON p then
    let t = typ p in
    let s = catnspans e t in
    mk s (EXConstraint (e, t))
  else
    e

and infix_expression p prec : expr node =
  if prec >= 0 then
    (* TODO: allow right associativity? *)
    let rec go lhs = 
      if matschs (allowed_infix_operators prec) p then
        let op = mk_infix_operator p.previous in
        let rhs = infix_expression p (prec - 1) in
        let s = catnspans lhs rhs in
        go (mk_g s (EXApplication (op, [lhs; rhs])))
      else
        lhs
    in
    go (infix_expression p (prec - 1))
  else
    unary_expression p

and unary_expression p : expr node =
  if matschs (allowed_unary_operators ()) p then
    let op = mk_unary_operator p.previous in
    let e = unary_expression p in
    let s = catnspans op e in
    mk_g s (EXApplication (op, [e]))
  else
    application_expression p

and application_expression p : expr node =
  (* first try construct *)
  try try_parse p begin fun p ->
    let ident = upper_longident p false in
    let es =
      try
        let e = try_parse p (atom_expression false) in
        let es = many p (matsch SEMICOLON) (atom_expression true) in
        e :: es
      with Backtrack -> []
    in
    mk ident.span (EXConstruct (ident, es))

  end with Backtrack ->
    (* otherwise maybe application *)
    let e = atom_expression true p in
    let es = many_try p (atom_expression false) in
    if es <> [] then
      let s = catnspans e (List.hd es) in
      mk s (EXApplication (e, es))
    else e

and atom_expression must p : expr node =
  let mk' i = mk_t i (advance p) in
  match current_t p with
    | INTEGER i -> mk' (EXLiteral (LIInt i))
    | FLOAT f -> mk' (EXLiteral (LIFloat f))
    | CHARACTER c -> mk' (EXLiteral (LIChar c))
    | STRING s -> mk' (EXLiteral (LIString s))
    | EMPTYPARENS -> mk' (EXLiteral LINil)
    
    | t when tokens_eq t (dummy `MAGICNAME) ->
      mk' (EXMagical (unpack_str t))
    | t when tokens_eq t (dummy `LOWERNAME) ->
      let i = lower_longident p true in
      mk i.span (EXIdentifier i)

    | LPAREN -> begin
        let start_s = (advance p).span in
        let e = expression p in
        if not (matsch RPAREN p) then
          unclosed "'('" start_s "the closing ')' here" (current p).span;

        let s = catspans start_s p.previous.span in
        let e' = if groups then EXGrouping e else e.item in
        mk s e'
      end

    (* TODO: lists and whatnot *)

    | _ ->
      if must then error_at_current p (Expected "an expression") []
      else backtrack ()

(* ====================== expression helpers ===================== *)

and max_precedence = 5
and infix_operators =
  [
    [STAR, "*"; SLASH, "/"; MODULO, "%"];
    [PLUS, "+"; MINUS, "-"];
    [GREATER, ">"; GREATEREQUAL, ">="; LESSER, "<"; LESSEREQUAL, "<="];
    [EQUAL, "="; NOTEQUAL, "/="];
    [DBL_AMPERSAND, "&&"];
    [DBL_PIPE, "||"]; 
  ]

and allowed_infix_operators prec =
  List.nth infix_operators prec
  |> List.map fst
and mk_infix_operator t : expr node =
  let name =
    let rec go = function
      | (op, (name : string)) :: _ when op = t.typ -> name
      | _ :: ops -> go ops
      | [] -> failwith "mk_infix_operator"
    in go (List.flatten infix_operators)
  in
  let op = mk t.span (Ident.Ident name) in
  mk t.span (EXIdentifier op)

and unary_operators = [BANG, "~!"; PLUS, "~+"; MINUS, "~-"]

and allowed_unary_operators _ = List.map fst unary_operators
and mk_unary_operator t : expr node =
  let name =
    let find_fn (tt, _) = t.typ = tt in
    try snd (List.find find_fn unary_operators)
    with Not_found -> failwith "mk_unary_operator"
  in
  let op = mk t.span (Ident.Ident name) in
  mk t.span (EXIdentifier op)

(* ============================= types ============================ *)

and typ p : typ node = forall_type p

and forall_type p : typ node =
  try try_parse p begin fun p ->
    let start_s = (current p).span in
    let parsefn p = mk_t (unpack_str p.previous.typ) p.previous in
    let args = many p (matsch (dummy `PRIMENAME)) parsefn in
    if not (matsch DOT p) then backtrack ();
    let ty = forall_type p in
    mk (catspans start_s ty.span) (TYForall (args, ty))
  end with Backtrack ->
    function_type p

and function_type p : typ node =
  let mk_fn lhs rhs s = mk s (TYFunction (lhs, rhs)) in
  left_assoc p tuple_type ARROW mk_fn

and tuple_type p : typ node =
  let t = effect_type p in
  let ts = many p (matsch SEMICOLON) effect_type in

  if ts <> [] then
    let s = catnspans t (List.hd ts) in
    mk s (TYTuple (t :: ts))
  else t

(* TODO: constructed types *)

and effect_type p : typ node =
  if matsch BANG p then
    let b_span = p.previous.span in
    let t = effect_type p in
    let s = catspans b_span t.span in
    mk s (TYEffect t)
  else
    atom_type p

and atom_type p : typ node =
  if matschs [dummy `BUILTINITY; dummy `BUILTINFTY; BUILTINVTY] p then
    mk p.previous.span (TYPrimitive (unpack_typ p.previous.typ))

  else if matsch (dummy `PRIMENAME) p then
    mk p.previous.span (TYVariable (unpack_str p.previous.typ))

  else try try_parse p begin fun p ->
    let i = upper_longident p false in
    mk i.span (TYConstruct (None, i))
  end with Backtrack ->
    error_at_current p (Expected "a type") []

(* ========================== identifiers ========================= *)

and longident_path p : string node list =
  let checkfn p =
    check (dummy `LOWERNAME) p
    && check_peek DBL_COLON p
  in
  let parsefn p =
    let tok = advance p in
    let t = unpack_str tok.typ in
    advance p &> mk_t t tok
  in
  many p checkfn parsefn

and longident kind p must =
  let start_s = (current p).span in
  let path = nmapi (longident_path p) in
  if check (dummy kind) p then
    let i = advance p in
    let s = catspans start_s i.span in
    mk s (Ident.from_list (path @ [unpack_str i.typ]))
  else
    if must then error_at_current p (Expected "an identifier") []
    else backtrack ()
  
and lower_longident p must = longident `LOWERNAME p must
and upper_longident p must = longident `UPPERNAME p must