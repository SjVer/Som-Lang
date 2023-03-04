open Parse.Ast

let use_of_unbound_error what ident span : unit =
  let open Report.Error in
  let ident' = Ident.to_string ident in
  let e = Type_error (Use_of_unbound (what, ident')) in
  (* TODO: error is not fatal now? *)
  (* raise Not_found *)
  Report.report (Report.make_error e (Some span))

let rec resolve_type ctx typ =
  let go = resolve_type ctx in
  let item = match typ.item with
    (* | TY_Variant vs ->
      let vs' = List.map (fun (n, ts) -> (canon_name' n, ts)) vs in
      TY_Variant vs' *)
    | TY_Grouping t -> TY_Grouping (go t)
    | TY_Forall (ps, t) -> TY_Forall (ps, go t)
    | TY_Effect t -> TY_Effect (go t) 
    | TY_Function (t1, t2) -> TY_Function (go t1, go t2)
    | TY_Tuple ts -> TY_Tuple (List.map (go) ts)

    | TY_Construct (t, i) -> begin try
        let i' = Context.lookup_qual_type_ident ctx i.item in
        Symbols.use (`Type i') i.span;
        TY_Construct (Option.map go t, {i with item = i'})
      with Not_found ->
        use_of_unbound_error "type" i.item i.span;
        TY_Any
      end

    | item -> item
  in
  {typ with item}

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
        Symbols.use (`Val i') i.span;
        EX_Construct ({i with item = i'}, List.map go es)
      with Not_found ->
        use_of_unbound_error "constructor" i.item i.span;
        EX_Error
      end
    | EX_Identifier i -> begin try
        let i' = Context.lookup_qual_value_ident ctx i.item in
        Symbols.use (`Val i') i.span;
        EX_Identifier {i with item = i'}
      with Not_found ->
        use_of_unbound_error "value" i.item i.span;
        EX_Error
      end

    | item -> item
  in
  {expr with item}

let rec resolve_toplevel ctx tl =
  let qualnode ctx n = {n with item = Context.qualify ctx n.item} in
  let mk item = {tl with item} in
  match tl.item with
    | TL_Value_Definition vdef ->
      (* not using a new context bc shadowing *)
      let vdef' =
        {
          vd_name = qualnode ctx vdef.vd_name;
          vd_expr = resolve_expr ctx vdef.vd_expr;
        }
      in
      let ctx' = Context.bind_value ctx vdef.vd_name.item vdef' in
      ctx', [mk (TL_Value_Definition vdef')]
    | TL_Type_Definition tdef ->
      let tdef' =
        {
          td_name = qualnode ctx tdef.td_name;
          td_params = tdef.td_params;
          td_type = resolve_type ctx tdef.td_type;
        }
      in
      let ctx' = Context.bind_type ctx tdef.td_name.item tdef' in
      ctx', [mk (TL_Type_Definition tdef')]
    | TL_Module (n, ast) ->
      let open Context in
      let subctx = {ctx with name = qualify ctx (Ident n.item)} in
      let subctx', ast' = resolve_ast subctx ast in
      let ctx' = Context.add_subcontext_prefixed ctx subctx' n.item in
      ctx', ast'
    | TL_Import _ -> ctx, []

and resolve_ast ctx : ast -> Context.t * ast = function
  | [] -> ctx, []
  | tl :: ast ->
    let ctx', tl' = resolve_toplevel ctx tl in
    let ctx'', ast' = resolve_ast ctx' ast in
    ctx'', tl' @ ast'

let resolve mod_name ast =
  let ctx = Context.empty mod_name in

  (* we keep all imported ast nodes seperate for now
     so that they don't get messed up by `resolve_ast` *)
  let ctx', imp_ast = Import.gather_and_apply_imports ctx ast in
  (* print_endline ("FINISHED HANDLING IMPORTS " ^ Ident.to_string ctx'.Context.name);
  Context.print ctx';
  print_newline (); *)

  (* we have the bindings of the imports in `ctx'` so we can
     just pretend everything is in place and resolve this ast *)
  let ctx'', ast' = resolve_ast ctx' ast in
  (* print_endline ("FINISHED RESOLVING " ^ Ident.to_string ctx''.name);
  Context.print ctx'';
  print_newline (); *)

  (* finally we do prepend the imported ast with this ast *)
  ctx'', imp_ast @ ast'