open Parse.Ast
open Report.Error
open Context

let use_of_unbound_error what ident span : unit =
  let ident' = Ident.to_string ident in
  let e = Type_error (Use_of_unbound (what, ident')) in
  (* TODO: error is not fatal now? *)
  (* raise Not_found *)
  Report.report (Report.make_error e (Some span))

let rec check_import_private = function
  | n :: path -> 
    if n.item.[0] = '_' then
      let e = Cannot_private ("import", n.item) in
      Report.make_error (Type_error e) (Some n.span)
      |> Report.report
    else
      check_import_private path
  | [] -> ()

let report_path_invalidity ctx path =
  let f (path_acc, _) path_seg =
    let path_acc' = path_acc @ [path_seg.item] in
    let path_ident = Ident.from_list path_acc' in
    if not (Context.check_subcontext ctx path_ident) then
      let mod_name = Ident.(from_list path_acc |> to_string) in
      let what =
        if List.length path_acc < List.length path - 1
        then "module" else "symbol"
      in
      let e = Has_no_symbol (mod_name, what, path_seg.item) in
      Report.make_error (Type_error e) (Some path_seg.span)
      |> Report.raise, false
    else
      path_acc', true
  in
  if snd (List.fold_left f ([], true) path) then
    (* path was empty or no invalidities were found *)
    let path' = String.concat "::" (nmapi path) in
    let span = Span.concat_spans
      (List.hd path).span
      (List.hd (List.rev path)).span
    in
    let e = Other_error (Other ("could not import " ^ path')) in
    Report.make_error e (Some span)
    |> Report.raise
  else
    failwith "unreachable"

let string_nodes_to_marked_ident path =
  let path' = nmapi path in
  let path' = ("#" ^ List.hd path') :: List.tl path' in
  Ident.from_list path'

let unmark_string str =
  if str.[0] = '#' then
    String.sub str 1 (String.length str - 1)
  else str

(* resolving stuff *)

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

let resolve_complex_type ctx cmplxtyp =
  let ctx', item = match cmplxtyp.item with
    | CTVariant rows ->
      let rec resolve_rows ctx = function
        | (i, ts) :: rest ->
          let ts' = List.map (resolve_type ctx) ts in
          let qi = Context.qualify ctx i.item in
          let ctx' = Context.bind_value ctx i.item qi in
          let ctx'', rest' = resolve_rows ctx' rest in
          ctx'', ({i with item = qi}, ts') :: rest'
        | [] -> ctx, []
      in
      let ctx', rows' = resolve_rows ctx rows in
      ctx', CTVariant rows'
    | CTSimple t -> ctx, CTSimple (resolve_type ctx t)
  in
  ctx', {cmplxtyp with item}

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

(* import stuff *)

let import_symbol ctx path dest_ident =
  (* preprend the '#' marking an imported module *)
  let src_ident = string_nodes_to_marked_ident path in
  try
    (* try as value first *)
    let qual_ident = lookup_qual_value_ident ctx src_ident in
    check_import_private path;
    bind_value ctx dest_ident qual_ident
  with Not_found -> try
    (* then try as type *)
    let qual_ident = lookup_qual_type_ident ctx src_ident in
    check_import_private path;
    Context.bind_type ctx dest_ident qual_ident
  with Not_found ->
    (* TODO: try as module here *)
    report_path_invalidity ctx path

let rec resolve_import ctx path kind =
  match kind.item with
    | IK_Module ->
      let old_ident = string_nodes_to_marked_ident path in
      let new_ident = Ident.from_list (nmapi path) in

      (* remove the '#' from all the bindings of the module *)
      let mod_ctx = extract_subcontext ctx old_ident in
      let f i q m = IMap.add Ident.(prepend new_ident i) q m in
      let ctx =
        {
          ctx with
          value_map = IMap.fold f mod_ctx.value_map ctx.value_map;
          type_map = IMap.fold f mod_ctx.type_map ctx.type_map;
        }
      in
      ctx

    | IK_Simple sym_path ->
      let path = path @ sym_path in
      let ident = Ident.Ident (List.hd (List.rev path)).item in
      import_symbol ctx path ident

    | IK_Glob -> failwith "resolve IK_Glob"

    | IK_Rename (sym_path, name) ->
      let path = path @ sym_path in
      let ident = Ident.Ident name.item in
      import_symbol ctx path ident

    | IK_Nested kinds ->
      let f ctx kind = resolve_import ctx path kind in
      List.fold_left f ctx kinds

(* ast stuff *)

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
      (* NOTE: types are always recursive now *)
      let td_name = qualnode ctx tdef.td_name in
      let ctx = Context.bind_type ctx tdef.td_name.item td_name.item in

      let ctx, type' = resolve_complex_type ctx tdef.td_type in
      let tdef' =
        {
          td_params = tdef.td_params;
          td_name;
          td_type = type';
        }
      in
      ctx, [mk (TLTypeDef tdef')]

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
      let ctx = Context.bind_value ctx
        edef.ed_name.item
        edef'.ed_name.item
      in
      ctx, [mk (TLExternDef edef')]

    | TLModule (n, ast) ->
      (* remove prefix '#' *)
      let name = unmark_string n.item in
      (* resolve module *)
      let subctx = {ctx with name = qualify ctx (Ident name)} in
      let subctx, ast = resolve_ast subctx ast in
      let ctx = add_subcontext_prefixed ctx subctx (Ident n.item) in
      ctx, ast

    | TLImport imp ->
      let ctx = resolve_import ctx imp.i_path imp.i_kind in
      ctx, []

and resolve_ast ctx = function
  | [] -> ctx, []
  | tl :: ast ->
    let ctx, tl = resolve_toplevel ctx tl in
    let ctx, ast = resolve_ast ctx ast in
    ctx, tl @ ast
