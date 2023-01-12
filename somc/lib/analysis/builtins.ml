open Parse.Ast
open Parse.Ident

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
    | EX_Grouping e -> EX_Grouping (rename_expr e)
    | EX_Binding (b, e) ->
      let b' = {b with expr = rename_expr b.expr} in
      let e' = rename_expr e in
      EX_Binding (b', e')
    | EX_Lambda b ->
      let b' = {b with expr = rename_expr b.expr} in
      EX_Lambda b'
    | EX_Sequence (e1, e2) ->
      EX_Sequence (rename_expr e1, rename_expr e2)
    | EX_Constraint (e, t) -> EX_Constraint (rename_expr e, t)
    | EX_Application (f, es) ->
      let es' = List.map rename_expr es in
      EX_Application (rename_expr f, es')
    | EX_Tuple es -> EX_Tuple (List.map rename_expr es)
    | EX_Construct (i, es) ->
      EX_Construct (rename_ident i, List.map rename_expr es)
    | EX_Identifier i -> EX_Identifier (rename_ident i)
    | _ -> e.item
  in
  {e with item}

let rec rename_builtins ast =
  let go tl =
    let item = match tl.item with
      | TL_Definition b ->
        TL_Definition {b with expr = rename_expr b.expr}
      | TL_Module (n, ast) ->
        TL_Module (n, rename_builtins ast)
      | _ as tl -> tl
    in
    {tl with item}
  in
  List.map go ast