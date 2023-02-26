open Parse.Ast

let use_of_unbound_error what ident span : unit =
  let open Report.Error in
  let ident' = Ident.to_string ident in
  let e = Type_error (Use_of_unbound (what, ident')) in
  Report.report (Report.make_error e (Some span));
  (* TODO: error is not fatal now? *)
  raise Not_found

let rec resolve_expr ctx expr =
  let go = resolve_expr ctx in
  let item = match expr.item with
    | EX_Grouping e -> EX_Grouping (go e)
    (* TODO: canon patt? *)
    | EX_Binding (b, e) -> EX_Binding ({b with vb_expr = go b.vb_expr}, go e)
    | EX_Lambda b -> EX_Lambda {b with vb_expr = go b.vb_expr}
    | EX_Sequence (e1, e2) -> EX_Sequence (go e1, go e2)
    (* | EX_Constraint (e, t) -> EX_Constraint (go e, canon_type ctx t) *)
    | EX_Application (e, es) -> EX_Application (go e, List.map go es)
    | EX_Tuple es -> EX_Tuple (List.map go es)

    | EX_Construct (i, es) -> begin try
        let i' = Context.lookup_qual_value_ident ctx i.item in
        Symboltable.use_value ctx.table i' i.span;
        EX_Construct ({i with item = i'}, List.map go es)
      with Not_found ->
        use_of_unbound_error "constructor" i.item i.span;
        EX_Error
      end
    | EX_Identifier i -> begin try
      let i' = Context.lookup_qual_value_ident ctx i.item in
      Symboltable.use_value ctx.table i' i.span;
        EX_Identifier {i with item = i'}
      with Not_found ->
        use_of_unbound_error "value" i.item i.span;
        EX_Error
      end
    | item -> item
  in
  {expr with item}

let rec resolve_toplevel ctx tl =
  match tl.item with
    | TL_Value_Definition vdef ->
      (* not using a new context bc shadowing *)
      let vdef' = {vdef with vd_expr = resolve_expr ctx vdef.vd_expr} in
      Context.add_local_value ctx vdef'
    | TL_Module (n, ast) ->
      let open Context in
      let ctx' = {ctx with name = qualify ctx (Ident n.item)} in
      let ctx'' = resolve_ast ctx' ast in
      {
        ctx with
        table = Symboltable.merge_tables ctx.table ctx''.table;
        subcontexts = ctx.subcontexts @ ctx''.subcontexts @ [ctx''.name];
      }
    | _ -> ctx

and resolve_ast ctx = function
  | [] -> ctx
  | tl :: ast ->
    let ctx' = resolve_toplevel ctx tl in
    resolve_ast ctx' ast

let resolve mod_name ast : Context.ast_symbol_table =
  let ctx = Context.empty mod_name in
  let ctx' = Import.gather_and_apply_imports ctx ast in
  (* print_endline ("FINISHED IMPORTING " ^ Ident.to_string ctx'.Context.name);
  Context.print ctx';
  print_newline (); *)
  let ctx'' = resolve_ast ctx' ast in
  ctx''.table