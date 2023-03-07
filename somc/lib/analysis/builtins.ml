open Parse.Ast
open Symbols.Ident

let rename_ident node =
  let op str = from_list ["_std_ops"; str] in
  let renamed, item = match node.item with
    | Ident "::" -> true, from_list ["_std_list"; "Cons"]
    | Ident "[]" -> true, from_list ["_std_list"; "Nil"]
    | Ident "^"  -> true, op "pow"
    | Ident "*"  -> true, op "mul"
    | Ident "/"  -> true, op "div"
    | Ident "%"  -> true, op "mod"
    | Ident "+"  -> true, op "add"
    | Ident "-"  -> true, op "sub"
    | Ident ">"  -> true, op "gr"
    | Ident ">=" -> true, op "greq"
    | Ident "<"  -> true, op "ls"
    | Ident "<=" -> true, op "lseq"
    | Ident "="  -> true, op "eq"
    | Ident "/=" -> true, op "neq"
    | Ident "&&" -> true, op "and"
    | Ident "^^" -> true, op "xor"
    | Ident "||" -> true, op "or"
    | _ as i -> false, i
  in
  let ghost = node.span.ghost || renamed in
  {
    span = {node.span with ghost};
    item;
  }

let rec rename_expr e =
  let item = match e.item with
    | EXGrouping e -> EXGrouping (rename_expr e)
    | EXBinding (b, e) ->
      let b' = {b with vb_expr = rename_expr b.vb_expr} in
      let e' = rename_expr e in
      EXBinding (b', e')
    | EXLambda {vb_patt; vb_expr} ->
      let b' = {vb_patt;  vb_expr = rename_expr vb_expr} in
      EXLambda b'
    | EXSequence (e1, e2) ->
      EXSequence (rename_expr e1, rename_expr e2)
    | EXConstraint (e, t) -> EXConstraint (rename_expr e, t)
    | EXApplication (f, es) ->
      let es' = List.map rename_expr es in
      EXApplication (rename_expr f, es')
    | EXTuple es -> EXTuple (List.map rename_expr es)
    | EXConstruct (i, es) ->
      EXConstruct (rename_ident i, List.map rename_expr es)
    | EXIdentifier i -> EXIdentifier (rename_ident i)
    | _ -> e.item
  in
  {e with item}

let rec rename_builtins (ast : ast) : ast =
  let go tl =
    let item = match tl.item with
      | TLValueDef {vd_name; vd_expr} ->
        TLValueDef {vd_name; vd_expr = rename_expr vd_expr}
      | TLModule (n, ast) ->
        TLModule (n, rename_builtins ast)
      | _ as tl -> tl
    in
    {tl with item}
  in
  List.map go ast