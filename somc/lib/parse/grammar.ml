open Report.Error
open Parser
open Handling
open Token
open Ast

(* =========================== helpers ============================ *)

(* if true produce stuff like Pexp_grouping *)
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
  Ptl_module (name, tls)

and toplevel_import_normal p =
  i (advance p);
  let i_path = finish_import_path p in
  Ptl_import { i_path; i_kind = mk_t Pik_module p.previous }

and toplevel_import_from p =
  i (advance p);
  let i_path = finish_import_path p in
  i (consume USE "\"use\"" p);
  let start_s = (current p).span in
  let kind =
    (* `use *` or `use ...` *)
    if matsch STAR p then Pik_glob
    else finish_import_from p
  in
  let s = catspans start_s p.previous.span in
  Ptl_import { i_path; i_kind = mk s kind}

and toplevel_value_definition p =
  i (advance p);
  let name = consume_ident p in
  let vd_name = {name with item = Ident.Ident name.item} in
  
  (* expression *)
  let (t, e) = strict_binding p expression EQUAL "=" in
  Ptl_value_def {vd_name; vd_expr = wrap_type_constraint t e}

and toplevel_type_definition p =
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
    if matsch IS p then
      let t = typ p in
      mk t.span (Pct_simple t)
    else begin
      i (consume OF "\"is\" or \"of\"" p);
      let start_s = (current p).span in
      let typ = finish_complex_type p in
      mk (catspans start_s p.previous.span) typ
    end
  in

  (* wrap type in Pty_forall if there's parameters *)
  Ptl_type_def {td_params; td_name; td_type}

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
    try_and_skip_until p pattern [COLON] Ppat_wildcard
    |> ignore
  done;
  
  (* type annotation is not optional *)
  i (consume COLON "\":\"" p);
  let ed_type = typ p in

  Ptl_extern_def {ed_native_name; ed_name; ed_type}

(* ======================= toplevel helpers ====================== *)

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
    mk_t' p (Pik_simple (path @ [ident]))
  in
  (* at least one nested import, maybe more *)
  let first = go p in
  let other = many p (matsch COMMA) go in
  Pik_nested (first :: other)

and finish_import_path p =
  let path = longident_path p in
  let last = consume (dummy `LOWERNAME) "an identifier" p in
  path @ [mk_t (unpack_str last.typ) last]

and finish_complex_type p =
  if matsch PIPE p || check (dummy `UPPERNAME) p then
    Pct_variant (finish_variant_type p)
  else
    failwith "TODO: parse other complex types" 

and finish_variant_type p =
  (* first (optional) '|' already consumed *)
  let ident =
    let tok = consume (dummy `UPPERNAME) "an uppercase identifier" p in
    mk tok.span (Ident.Ident (unpack_str tok.typ))
  in
  let args =
    try
      let t = constructed_type false p in
      let ts = many p (matsch SEMICOLON) (constructed_type false) in
      t :: ts
    with Backtrack -> []
  in

  if matsch PIPE p then
    (ident, args) :: finish_variant_type p
  else [ident, args]

(* ============================ binding =========================== *)

and binding (p : parser) exprfn sep sepstr : value_binding =
  let vb_patt = try_and_skip_until p pattern [sep; COLON] Ppat_wildcard in
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
    let vb_patt = try_and_skip_until p pattern [sep; COLON] Ppat_wildcard in
    let (t, vb_expr) = strict_binding p exprfn sep sepstr in
    let s = catnspans vb_patt vb_expr in
    t, mk_g s (Pexp_lambda {vb_patt; vb_expr})

and wrap_type_constraint t e =
  match t with
    | Some t -> {
        span = {e.span with ghost = true};
        item = Pexp_constraint (e, t);
      }
    | None -> e

(* ============================ patterns ========================== *)

and pattern p = tuple_pattern p

and tuple_pattern p =
  let pt = constructor_pattern p in
  let pts = many p (matsch SEMICOLON) constructor_pattern in

  if pts <> [] then
    let s = catnspans pt (List.hd pts) in
    mk s (Ppat_tuple (pt :: pts))
  else pt

and constructor_pattern p =
  try try_parse p begin fun p ->
    let ident = upper_longident p false in
    let args, span =
      try
        let arg = try_parse p (atom_pattern false) in
        let args = arg :: many p (matsch SEMICOLON) (atom_pattern true) in
        args, catnspans ident (List.hd (List.rev args)) 
      with Backtrack -> [], ident.span
    in
    mk span (Ppat_construct (ident, args))
  end with Backtrack ->
    atom_pattern true p

and atom_pattern must p =
  let curr = current p in
  let mk' i = mk curr.span i in
  
  if matsch (dummy `LOWERNAME) p then mk' (Ppat_variable (unpack_str curr.typ))
  else if matsch UNDERSCORE p then mk' Ppat_wildcard
  else if matsch LPAREN p then
    let start_s = p.previous.span in
    let e = pattern p in
    if not (matsch RPAREN p) then
      unclosed "'('" start_s "the closing ')' here" (current p).span;
    let s = catspans start_s p.previous.span in
    mk s e.item
  else
    let mk' i = mk_t i (advance p) in
    match current_t p with
      | INTEGER i -> mk' (Ppat_literal (Pli_int i))
      | FLOAT f -> mk' (Ppat_literal (Pli_float f))
      | CHARACTER c -> mk' (Ppat_literal (Pli_char c))
      | STRING s -> mk' (Ppat_literal (Pli_string s))
      | EMPTYPARENS -> mk' (Ppat_literal Pli_null)
      | _ ->
        if must then error_at_current p (Expected "a pattern") []
        else backtrack ()

(* =========================== expression ========================= *)

and expression p = keyword_expression p

and keyword_expression p =
  match current_t p with
    | LET ->
      let let_s = (advance p).span in
      let bind = binding p keyword_expression EQUAL "=" in
      i (consume IN "\"in\"" p);
      let body = keyword_expression p in
      let s = catspans let_s body.span in 
      mk s (Pexp_binding (bind, body))

    | MATCH ->
      let match_s = (advance p).span in
      let e = sequence_expression p in
      ignore (consume PIPE "'|'" p);
      let cases = match_cases p in
      (* let s = *)
      let s = match_s in
      mk s (Pexp_match (e, cases))

    | SWITCH ->
      let switch_s = (advance p).span in
      ignore (matsch PIPE p);
      let cases = match_cases p in
      (* let s = catspans switch_s  *)
      let s = switch_s in
      mk s (Pexp_switch cases)

    | IF ->
      let start_s = (advance p).span in
      let cond = sequence_expression p in
      ignore (consume THEN "'then'" p);
      let texp = sequence_expression p in
      ignore (consume ELSE "'else'" p);
      let eexp = sequence_expression p in
      
      let s = catspans start_s eexp.span in
      mk s (Pexp_if (cond, texp, eexp))

    | _ -> sequence_expression p

and sequence_expression p =
  let mk_seq e1 e2 s = mk s (Pexp_sequence (e1, e2)) in
  left_assoc p lambda_expression COMMA mk_seq

and lambda_expression p =
  if matsch LAM p then begin
    let start_s = p.previous.span in
    let bind = binding p tuple_expression ARROW "->" in
    let s = catspans start_s bind.vb_expr.span in
    mk s (Pexp_lambda bind)
  end else tuple_expression p

and tuple_expression p =
  let e = t_constr_expression p in
  let es = many p (matsch SEMICOLON) t_constr_expression in

  if es <> [] then
    let s = catnspans e (List.hd es) in
    mk s (Pexp_tuple (e :: es))
  else e

and t_constr_expression p =
  let e = infix_expression p max_precedence in
  if matsch COLON p then
    let t = typ p in
    let s = catnspans e t in
    mk s (Pexp_constraint (e, t))
  else
    e

and infix_expression p prec =
  if prec >= 0 then
    (* TODO: allow right associativity? *)
    let rec go lhs = 
      if matschs (allowed_infix_operators prec) p then
        let op = mk_infix_operator p.previous in
        let rhs = infix_expression p (prec - 1) in
        let s = catnspans lhs rhs in
        go (mk_g s (Pexp_apply (op, [lhs; rhs])))
      else
        lhs
    in
    go (infix_expression p (prec - 1))
  else
    unary_expression p

and unary_expression p =
  if matschs (allowed_unary_operators ()) p then
    let op = mk_unary_operator p.previous in
    let e = unary_expression p in
    let s = catnspans op e in
    mk_g s (Pexp_apply (op, [e]))
  else
    application_expression p

and application_expression p =
  (* first try construct *)
  try try_parse p begin fun p ->
    (* TODO: because the parsing of the upper ident
       is 'above' atom_expression we cannot parse
       stuff like `Nil` in `List 123 ; Nil`. *)
    let ident = upper_longident p false in
    let e, span =
      try
        let e = try_parse p (atom_expression false) in
        [e], catnspans ident e
      with Backtrack ->
        [], ident.span
    in
    mk span (Pexp_construct (ident, e))

  end with Backtrack ->
    (* otherwise maybe application *)
    let e = atom_expression true p in
    let es = many_try p (atom_expression false) in
    if es <> [] then
      let s = catnspans e (List.hd (List.rev es)) in
      mk s (Pexp_apply (e, es))
    else e

and atom_expression must p =
  let mk' i = mk_t i (advance p) in
  match current_t p with
    | INTEGER i -> mk' (Pexp_literal (Pli_int i))
    | FLOAT f -> mk' (Pexp_literal (Pli_float f))
    | CHARACTER c -> mk' (Pexp_literal (Pli_char c))
    | STRING s -> mk' (Pexp_literal (Pli_string s))
    | EMPTYPARENS -> mk' (Pexp_literal Pli_null)
    
    | t when tokens_eq t (dummy `MAGICNAME) ->
      mk' (Pexp_primitive (unpack_str t))
    | t when tokens_eq t (dummy `LOWERNAME) ->
      let i = lower_longident p true in
      mk i.span (Pexp_ident i)

    | LPAREN -> begin
        let start_s = (advance p).span in
        let e = expression p in
        if not (matsch RPAREN p) then
          unclosed "'('" start_s "the closing ')' here" (current p).span;

        let s = catspans start_s p.previous.span in
        mk s (if groups then Pexp_grouping e else e.item)
      end

    (* TODO: lists and whatnot *)

    | _ ->
      if must then error_at_current p (Expected "an expression") []
      else backtrack ()

(* ====================== expression helpers ===================== *)

and match_cases p =
  (* first '|' alraedy handled *)
  let patt = pattern p in
  ignore (consume ARROW "'->'" p);
  let e = sequence_expression p in

  if not (matsch END p) && matsch PIPE p then
    (patt, e) :: match_cases p
  else [patt, e]

and max_precedence = 5
and infix_operators =
  [
    [
      STAR, INFIX_OP_1 "*";
      SLASH, INFIX_OP_1 "/";
      MODULO, INFIX_OP_1 "%"
    ];
    [
      PLUS, INFIX_OP_2 "+";
      MINUS, INFIX_OP_2 "-";
    ];
    [
      GREATER, INFIX_OP_3 ">";
      GREATEREQUAL, INFIX_OP_3 ">=";
      LESSER, INFIX_OP_3 "<";
      LESSEREQUAL, INFIX_OP_3 "<="
    ];
    [
      EQUAL, INFIX_OP_4 "=";
      NOTEQUAL, INFIX_OP_4 "/="
    ];
    [
      DBL_AMPERSAND, INFIX_OP_5 "&&"
    ];
    [
      DBL_PIPE, INFIX_OP_6 "||"
    ]; 
  ]

and allowed_infix_operators prec =
  let ops = 
    List.nth infix_operators prec
    |> List.map fst
  and other_op = List.nth [
      `INFIX_OP_1; `INFIX_OP_2; `INFIX_OP_3;
      `INFIX_OP_4; `INFIX_OP_5; `INFIX_OP_6
    ] prec |> dummy
  in
  other_op :: ops

and mk_infix_operator t =
  let op =
    match t.typ with 
      | INFIX_OP_1 _ | INFIX_OP_2 _ | INFIX_OP_4 _
      | INFIX_OP_3 _ | INFIX_OP_5 _ | INFIX_OP_6 _ ->
        t.typ
      | _ ->
        List.flatten infix_operators
        |> List.find (fun (typ, _) -> typ = t.typ)
        |> snd
  in
  let ident = Ident.Ident (unpack_str op) in
  mk t.span (Pexp_ident (mk t.span ident))

and unary_operators =
  [
    BANG, UNARY_OP "~!";
    PLUS, UNARY_OP "~+";
    MINUS, UNARY_OP "~-"
  ]

and allowed_unary_operators _ =
  let ops = List.map fst unary_operators
  and other_op = dummy `UNARY_OP in
  other_op :: ops

and mk_unary_operator t =
  let op =
    if tokens_eq t.typ (dummy `UNARY_OP) then
      t.typ
    else
      unary_operators
      |> List.find (fun (typ, _) -> typ = t.typ)
      |> snd
  in
  let ident = Ident.Ident (unpack_str op) in
  mk t.span (Pexp_ident (mk t.span ident))

(* ============================= types ============================ *)

and typ p = forall_type p

and forall_type p =
  try try_parse p begin fun p ->
    let start_s = (current p).span in
    let parsefn p = mk_t (unpack_str p.previous.typ) p.previous in
    let args = many p (matsch (dummy `PRIMENAME)) parsefn in
    if not (matsch DOT p) then backtrack ();
    let ty = forall_type p in
    mk (catspans start_s ty.span) (Pty_forall (args, ty))
  end with Backtrack ->
    function_type p

and function_type p =
  (* let mk_fn lhs rhs s = mk s (Pty_function (lhs, rhs)) in
  left_assoc p tuple_type ARROW mk_fn *)
  let lhs = tuple_type p in
  if matsch ARROW p then
    let rhs = function_type p in
    mk (catnspans lhs rhs) (Pty_function (lhs, rhs))
  else lhs

and tuple_type p =
  let t = constructed_type true p in
  let ts = many p (matsch SEMICOLON) (constructed_type true) in

  if ts <> [] then
    let s = catnspans t (List.hd ts) in
    mk s (Pty_tuple (t :: ts))
  else t

and constructed_type must p =
  (* TODO: `A B` works, but `A B C` only works as `(A B) C` *)
  let t = effect_type must p in
  try
    let i = upper_longident p false in
    mk (catnspans t i) (Pty_construct (Some t, i))
  with Backtrack -> t

and effect_type must p =
  if matsch BANG p then
    let b_span = p.previous.span in
    let t = effect_type true p in
    let s = catspans b_span t.span in
    mk s (Pty_effect t)
  else
    atom_type must p

and atom_type must p =
  if matsch LPAREN p then begin
    let start_s = p.previous.span in
    let t = typ p in
    if not (matsch RPAREN p) then
      unclosed "'('" start_s "the closing ')' here" (current p).span;

    let s = catspans start_s p.previous.span in
    mk s (if groups then Pty_grouping t else t.item)
  end else if matsch (dummy `PRIMENAME) p then
    let s = mk p.previous.span (unpack_str p.previous.typ) in
    mk s.span (Pty_variable s)
  else try try_parse p begin fun p ->
    let i = upper_longident p false in
    mk i.span (Pty_construct (None, i))
  end with Backtrack ->
    if must then error_at_current p (Expected "a type") []
    else backtrack ()

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
