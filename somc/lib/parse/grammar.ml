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

(* =========================== toplevel =========================== *)

let rec prog p =
  let tls = many p not_at_end toplevel in
  consume EOF "end of file" p &>
  tls

and toplevel p : unit =
  let e = expression p in
  Print_ast.print_expr_node e

(* ============================ binding =========================== *)

and binding (p : parser) exprfn sep sepstr : value_binding =
  let patt = pattern p in
  let (t, e) = strict_binding p exprfn sep sepstr in

  (* put in the ghost EX_Constraint *)
  let expr = match t with
    | Some t -> {
        span = {e.span with ghost = true};
        item = EX_Constraint (e, t);
      }
    | None -> e
  in
  {patt; expr}

and strict_binding p exprfn sep sepstr =
  if checks [COLON; sep] p then begin
    (* type annotation *)
    let t = if matsch COLON p
      then Some (typ p)
      else None
    in
    (* sep and body *)
    i (consume sep ("a pattern or \"" ^ sepstr ^ "\"") p);
    let e = exprfn p in
    t, e
  end else begin
    (* another pattern *)
    let patt = pattern p in
    let (t, expr) = strict_binding p exprfn sep sepstr in
    let s = catnspans patt expr in
    t, mk_g s (EX_Lambda {patt; expr})
  end

(* ============================ patterns ========================== *)

and pattern p : pattern node =
  (* TODO *)
  atom_pattern p

and atom_pattern p : pattern node =
  let curr = current p in
  let mk' i = mk curr.span i in
  
  if matsch (dummy `LOWERNAME) p then mk' (PA_Variable (unpack_str curr.typ))
  else if matsch UNDERSCORE p then mk' PA_Wildcard
  else fail ()

(* =========================== expression ========================= *)

and expression p : expr node = let_expression p

and let_expression p : expr node =
  if matsch LET p then begin
    let let_s = p.previous.span in
    let bind = binding p let_expression EQUAL "=" in
    i (consume IN "\"in\"" p);
    let body = let_expression p in
    let s = Span.concat_spans let_s body.span in 
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
    let s = Span.concat_spans start_s bind.expr.span in
    mk s (EX_Lambda bind)
  end else tuple_expression p

and tuple_expression p : expr node =
  let e = t_constr_expression p in
  let parsefn p = i (advance p); t_constr_expression p in
  let es = many p (check SEMICOLON) parsefn in

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
  (* TODO *)
  atom_expression p 

and atom_expression p : expr node =
  let mk' i = mk_t i (advance p) in
  match current_t p with
    | INTEGER i -> mk' (EX_Literal (LI_Int i))
    | FLOAT f -> mk' (EX_Literal (LI_Float f))
    | CHARACTER c -> mk' (EX_Literal (LI_Char c))
    | STRING s -> mk' (EX_Literal (LI_String s))
    | EMPTYPARENS -> mk' (EX_Literal LI_Nil)

    | LPAREN -> begin
        let start_s = (advance p).span in
        let e = expression p in
        if not (matsch RPAREN p) then fail ();

        let s = Span.concat_spans start_s p.previous.span in
        if groups then mk s (EX_Grouping e)
        else mk s e.item
      end

    (* TODO *)
    | _ -> error_at_current p Unexpected []

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

(* ============================= types ============================ *)

and typ p : typ node = function_type p

and function_type p : typ node =
  let mk_fn lhs rhs s = mk s (TY_Function (lhs, rhs)) in
  left_assoc p tuple_type ARROW mk_fn

and tuple_type p : typ node =
  let t = effect_type p in
  let parsefn p = i (advance p); effect_type p in
  let ts = many p (check SEMICOLON) parsefn in

  if ts <> [] then
    let s = catnspans t (List.hd ts) in
    mk s (TY_Tuple (t :: ts))
  else t

and effect_type p : typ node =
  if matsch BANG p then
    let b_span = p.previous.span in
    let t = effect_type p in
    let s = Span.concat_spans b_span t.span in
    mk s (TY_Effect t)
  else
    atom_type p

and atom_type p : typ node =
  let mk' i = mk_t i (advance p) in
  let builtin t = mk' (TY_Primitive (unpack_typ t)) in
  let curr_t = current_t p in

  if matsch (dummy `BUILTINITY) p then builtin curr_t
  else if matsch (dummy `BUILTINFTY) p then builtin curr_t
  else if matsch BUILTINVTY p then builtin curr_t
    (* TODO *)
  else fail ()

