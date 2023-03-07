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
    (* | TYVariant vs ->
      let vs' = List.map (fun (n, ts) -> (canon_name' n, ts)) vs in
      TYVariant vs' *)
    | TYGrouping t -> TYGrouping (go t)
    | TYForall (ps, t) -> TYForall (ps, go t)
    | TYEffect t -> TYEffect (go t) 
    | TYFunction (t1, t2) -> TYFunction (go t1, go t2)
    | TYTuple ts -> TYTuple (List.map (go) ts)

    | TYConstruct (t, i) -> begin try
        let i' = Context.lookup_qual_type_ident ctx i.item in
        Symbols.use (`Type i') i.span;
        TYConstruct (Option.map go t, {i with item = i'})
      with Not_found ->
        use_of_unbound_error "type" i.item i.span;
        TYAny
      end

    | item -> item
  in
  {typ with item}

let bind_pattern ctx = function
  | PAVariable n -> Context.bind_value ctx (Ident n) (Ident n)
  | PAWildcard -> ctx

let rec resolve_expr ctx expr =
  let go = resolve_expr ctx in
  let item = match expr.item with
    | EXGrouping e -> EXGrouping (go e)

    | EXBinding (b, e) ->
      let vb_expr = go b.vb_expr in
      let ctx' = bind_pattern ctx b.vb_patt.item in
      EXBinding ({b with vb_expr}, resolve_expr ctx' e)

    | EXLambda b ->
      let ctx' = bind_pattern ctx b.vb_patt.item in
      let vb_expr = resolve_expr ctx' b.vb_expr in
      EXLambda {b with vb_expr}

    | EXSequence (e1, e2) -> EXSequence (go e1, go e2)
    | EXConstraint (e, t) -> EXConstraint (go e, resolve_type ctx t)
    | EXApplication (e, es) -> EXApplication (go e, List.map go es)
    | EXTuple es -> EXTuple (List.map go es)

    | EXConstruct (i, es) -> begin try
        let i' = Context.lookup_qual_value_ident ctx i.item in
        Symbols.use (`Val i') i.span;
        EXConstruct ({i with item = i'}, List.map go es)
      with Not_found ->
        use_of_unbound_error "constructor" i.item i.span;
        EXError
      end
    | EXIdentifier i -> begin try
        let i' = Context.lookup_qual_value_ident ctx i.item in
        Symbols.use (`Val i') i.span;
        EXIdentifier {i with item = i'}
      with Not_found ->
        use_of_unbound_error "value" i.item i.span;
        EXError
      end

    | item -> item
  in
  {expr with item}

let rec resolve_toplevel ctx tl =
  let qualnode ctx n = {n with item = Context.qualify ctx n.item} in
  let mk item = {tl with item} in
  match tl.item with
    | TLValueDef vdef ->
      let vdef' =
        {
          vd_name = qualnode ctx vdef.vd_name;
          vd_expr = resolve_expr ctx vdef.vd_expr;
        }
      in
      let ctx' = Context.bind_value ctx
        vdef.vd_name.item
        vdef'.vd_name.item
      in
      ctx', [mk (TLValueDef vdef')]

    | TLTypeDef tdef ->
      let tdef' =
        {
          td_name = qualnode ctx tdef.td_name;
          td_type = resolve_type ctx tdef.td_type;
        }
      in
      let ctx' = Context.bind_type ctx
        tdef.td_name.item
        tdef'.td_name.item
      in
      ctx', [mk (TLTypeDef tdef')]

    | TLExternDef edef ->
      (* TODO: fix this if we know how *)
      let map_name i = Ident.Ident ("%" ^ i) in
      let edef' =
        {
          ed_native_name = edef.ed_native_name;
          (* ed_name = qualnode ctx edef.ed_name; *)
          ed_name = nmap edef.ed_native_name map_name;
          ed_type = resolve_type ctx edef.ed_type;
        }
      in
      let ctx' = Context.bind_value ctx
        edef.ed_name.item
        edef'.ed_name.item
      in
      ctx', [mk (TLExternDef edef')]

    | TLModule (n, ast) ->
      let open Context in
      let subctx = {ctx with name = qualify ctx (Ident n.item)} in
      let subctx', ast' = resolve_ast subctx ast in
      let ctx' = add_subcontext_prefixed ctx subctx' n.item in
      ctx', ast'

    | TLImport _ -> ctx, []

and resolve_ast ctx : ast -> Context.t * ast = function
  | [] -> ctx, []
  | tl :: ast ->
    let ctx', tl' = resolve_toplevel ctx tl in
    let ctx'', ast' = resolve_ast ctx' ast in
    ctx'', tl' @ ast'
