open Report.Error
open Parser
open Handling
open Token
open Ast

(* =========================== helpers ============================ *)

(* if true produce stuff like EX_Grouping *)
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
      skip_until [LET; TYPE; MOD; USE] p;
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
    | _ -> error_at_current p (Expected "a toplevel statement") []
  in
  let s = catspans start_s p.previous.span in
  mk s tl

(* TODO: `use ... as ...` *)

and toplevel_module p : toplevel =
  i (advance p);
  let name = consume_ident p in
  i (consume LBRACE "\"{\"" p);
  let tls = many p (fun p -> not (check RBRACE p)) toplevel in
  i (consume RBRACE "\"}\"" p);
  TL_Module (name, tls)

and toplevel_import_normal p : toplevel =
  i (advance p);
  let i_path = finish_import_path p in
  TL_Import { i_path; i_kind = mk_t IK_Module p.previous }

and toplevel_import_from p : toplevel =
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
  TL_Import { i_path; i_kind = mk s kind}

and toplevel_value_definition p : toplevel =
  i (advance p);
  let name = consume_ident p in
  let vd_name = {name with item = Ident.Ident name.item} in
  
  (* expression *)
  let (t, e) = strict_binding p expression EQUAL "=" in
  TL_Value_Definition {vd_name; vd_expr = wrap_type_constraint t e}

and toplevel_type_definition p : toplevel =
  i (advance p);
  (* params *)
  let parsefn p = mk_t (unpack_str p.previous.typ) p.previous in
  let td_params = many p (matsch (dummy `PRIMENAME)) parsefn in
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
  let td_type =
    if matsch IS p then typ p
    else begin
      i (consume OF "\"is\" or \"of\"" p);
      failwith "TODO: parse complex type definition"
    end
  in
  TL_Type_Definition {td_params; td_name; td_type}

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
  let vb_patt = try_and_skip_until p pattern [sep; COLON] PA_Wildcard in
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
    let vb_patt = try_and_skip_until p pattern [sep; COLON] PA_Wildcard in
    let (t, vb_expr) = strict_binding p exprfn sep sepstr in
    let s = catnspans vb_patt vb_expr in
    t, mk_g s (EX_Lambda {vb_patt; vb_expr})

and wrap_type_constraint t e =
  match t with
    | Some t -> {
        span = {e.span with ghost = true};
        item = EX_Constraint (e, t);
      }
    | None -> e

(* ============================ patterns ========================== *)

and pattern p : pattern node =
  (* TODO *)
  atom_pattern p

and atom_pattern p : pattern node =
  let curr = current p in
  let mk' i = mk curr.span i in
  
  if matsch (dummy `LOWERNAME) p then mk' (PA_Variable (unpack_str curr.typ))
  else if matsch UNDERSCORE p then mk' PA_Wildcard
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
    mk s (EX_Binding (bind, body))
  end
  else sequence_expression p

and sequence_expression p : expr node =
  let mk_seq e1 e2 s = mk s (EX_Sequence (e1, e2)) in
  left_assoc p lambda_expression COMMA mk_seq

and lambda_expression p : expr node =
  if matsch BACKSLASH p then begin
    let start_s = p.previous.span in
    let bind = binding p tuple_expression ARROW "->" in
    let s = catspans start_s bind.vb_expr.span in
    mk s (EX_Lambda bind)
  end else tuple_expression p

and tuple_expression p : expr node =
  let e = t_constr_expression p in
  let es = many p (matsch SEMICOLON) t_constr_expression in

  if es <> [] then
    let s = catnspans e (List.hd es) in
    mk s (EX_Tuple (e :: es))
  else e

and t_constr_expression p : expr node =
  let e = infix_expression p max_precedence in
  if matsch COLON p then
    let t = typ p in
    let s = catnspans e t in
    mk s (EX_Constraint (e, t))
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
        go (mk_g s (EX_Application (op, [lhs; rhs])))
      else
        lhs
    in
    go (infix_expression p (prec - 1))
  else
    base_expression p

and base_expression p : expr node =
  (* first try construct *)
  try try_parse p begin fun p ->
    let ident = upper_longident p false in
    let es =
      try
        let e = try_parse p (unary_expression false) in
        let es = many p (matsch SEMICOLON) (unary_expression true) in
        e :: es
      with Backtrack -> []
    in
    mk ident.span (EX_Construct (ident, es))

  end with Backtrack ->
    (* otherwise maybe application *)
    let e = unary_expression true p in
    let es = many_try p (unary_expression false) in
    if es <> [] then
      let s = catnspans e (List.hd es) in
      mk s (EX_Application (e, es))
    else e

and unary_expression must p : expr node =
  if matschs (allowed_unary_operators ()) p then
    let op = mk_unary_operator p.previous in
    let e = unary_expression must p in
    let s = catnspans op e in
    mk_g s (EX_Application (op, [e]))
  else
    atom_expression must p

and atom_expression must p : expr node =
  let mk' i = mk_t i (advance p) in
  match current_t p with
    | INTEGER i -> mk' (EX_Literal (LI_Int i))
    | FLOAT f -> mk' (EX_Literal (LI_Float f))
    | CHARACTER c -> mk' (EX_Literal (LI_Char c))
    | STRING s -> mk' (EX_Literal (LI_String s))
    | EMPTYPARENS -> mk' (EX_Literal LI_Nil)
    
    | t when tokens_eq t (dummy `EXTERNNAME) ->
      mk' (EX_External (unpack_str t))
    | t when tokens_eq t (dummy `MAGICNAME) ->
      mk' (EX_Magical (unpack_str t))
    | t when tokens_eq t (dummy `LOWERNAME) ->
      let i = lower_longident p true in
      mk i.span (EX_Identifier i)

    | LPAREN -> begin
        let start_s = (advance p).span in
        let e = expression p in
        if not (matsch RPAREN p) then fail ();

        let s = catspans start_s p.previous.span in
        let e' = if groups then EX_Grouping e else e.item in
        mk s e'
      end

    (* TODO *)
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
  mk t.span (EX_Identifier op)

and unary_operators = [BANG, "~!"; PLUS, "~+"; MINUS, "~-"]

and allowed_unary_operators _ = List.map fst unary_operators
and mk_unary_operator t : expr node =
  let name =
    let find_fn (tt, _) = t.typ = tt in
    try snd (List.find find_fn unary_operators)
    with Not_found -> failwith "mk_unary_operator"
  in
  let op = mk t.span (Ident.Ident name) in
  mk t.span (EX_Identifier op)

(* ============================= types ============================ *)

and typ p : typ node = forall_type p

and forall_type p : typ node =
  try try_parse p begin fun p ->
    let start_s = (current p).span in
    let parsefn p = mk_t (unpack_str p.previous.typ) p.previous in
    let args = many p (matsch (dummy `PRIMENAME)) parsefn in
    if not (matsch DOT p) then backtrack ();
    let ty = forall_type p in
    mk (catspans start_s ty.span) (TY_Forall (args, ty))
  end with Backtrack ->
    function_type p

and function_type p : typ node =
  let mk_fn lhs rhs s = mk s (TY_Function (lhs, rhs)) in
  left_assoc p tuple_type ARROW mk_fn

and tuple_type p : typ node =
  let t = effect_type p in
  let ts = many p (matsch SEMICOLON) effect_type in

  if ts <> [] then
    let s = catnspans t (List.hd ts) in
    mk s (TY_Tuple (t :: ts))
  else t

(* TODO: constructed types *)

and effect_type p : typ node =
  if matsch BANG p then
    let b_span = p.previous.span in
    let t = effect_type p in
    let s = catspans b_span t.span in
    mk s (TY_Effect t)
  else
    atom_type p

and atom_type p : typ node =
  if matschs [dummy `BUILTINITY; dummy `BUILTINFTY; BUILTINVTY] p then
    mk p.previous.span (TY_Primitive (unpack_typ p.previous.typ))

  else if matsch (dummy `PRIMENAME) p then
    mk p.previous.span (TY_Variable (unpack_str p.previous.typ))

  else try try_parse p begin fun p ->
    let i = upper_longident p false in
    mk i.span (TY_Construct (None, i))
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