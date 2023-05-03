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

let check_typedef_name span = function
  | Ident.Ident ("Int" | "Chr" | "Bln" | "Flt" | "Str" | "Nil") ->
    let e = Other_error (Other "cannot shadow builtin type") in
    Report.report (Report.make_error e (Some span))
  | _ -> ()

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
    | Pty_grouping t -> Pty_grouping (go t)
    | Pty_forall (ps, t) -> Pty_forall (ps, go t)
    | Pty_effect t -> Pty_effect (go t) 
    | Pty_function (t1, t2) -> Pty_function (go t1, go t2)
    | Pty_tuple ts -> Pty_tuple (List.map (go) ts)

    | Pty_construct (t, i) -> begin try
        let i' = Context.lookup_qual_type_ident ctx i.item in
        Symbols.use (`Type i') i.span;
        Pty_construct (Option.map go t, {i with item = i'})
      with Not_found ->
        use_of_unbound_error "type" i.item i.span;
        Pty_wildcard
      end

    | item -> item
  in
  {typ with item}

let resolve_complex_type ctx cmplxtyp =
  let ctx', item = match cmplxtyp.item with
    | Pct_variant rows ->
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
      ctx', Pct_variant rows'
    | Pct_simple t -> ctx, Pct_simple (resolve_type ctx t)
  in
  ctx', {cmplxtyp with item}

let rec bind_pattern ctx = function
  | Ppat_wildcard -> ctx
  | Ppat_variable n -> Context.bind_value ctx (Ident n) (Ident n)
  | Ppat_literal _ -> ctx
  | Ppat_construct (_, args) ->
    List.fold_left bind_pattern ctx (nmapi args) 
  | Ppat_tuple patts ->
    List.fold_left bind_pattern ctx (nmapi patts)

let rec resolve_cases ctx cases =
  let resolve (patt, expr) =
    let ctx = bind_pattern ctx patt.item in
    let expr = resolve_expr ctx expr in
    patt, expr
  in
  List.map resolve cases

and resolve_expr ctx expr =
  let go = resolve_expr ctx in
  let item = match expr.item with
    | Pexp_grouping e -> Pexp_grouping (go e)

    | Pexp_binding (b, e) ->
      let vb_expr = go b.vb_expr in
      let ctx' = bind_pattern ctx b.vb_patt.item in
      Pexp_binding ({b with vb_expr}, resolve_expr ctx' e)

    | Pexp_lambda b ->
      let ctx' = bind_pattern ctx b.vb_patt.item in
      let vb_expr = resolve_expr ctx' b.vb_expr in
      Pexp_lambda {b with vb_expr}

    | Pexp_match (scrut, cases) ->
      Pexp_match (go scrut, resolve_cases ctx cases)
    | Pexp_switch cases ->
      Pexp_switch (resolve_cases ctx cases)

    | Pexp_sequence (e1, e2) -> Pexp_sequence (go e1, go e2)
    | Pexp_constraint (e, t) -> Pexp_constraint (go e, resolve_type ctx t)
    | Pexp_apply (e, es) -> Pexp_apply (go e, List.map go es)
    | Pexp_tuple es -> Pexp_tuple (List.map go es)

    | Pexp_construct (i, es) -> begin try
        let i' = Context.lookup_qual_value_ident ctx i.item in
        Symbols.use (`Val i') i.span;
        Pexp_construct ({i with item = i'}, List.map go es)
      with Not_found ->
        use_of_unbound_error "constructor" i.item i.span;
        Pexp_error
      end
    | Pexp_ident i -> begin try
        let i' = Context.lookup_qual_value_ident ctx i.item in
        Symbols.use (`Val i') i.span;
        Pexp_ident {i with item = i'}
      with Not_found ->
        use_of_unbound_error "value" i.item i.span;
        Pexp_error
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
    | Pik_module ->
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

    | Pik_simple sym_path ->
      let path = path @ sym_path in
      let ident = Ident.Ident (List.hd (List.rev path)).item in
      import_symbol ctx path ident

    | Pik_glob ->
      let path_ident = string_nodes_to_marked_ident path in
      let mod_ctx = extract_subcontext ctx path_ident in
      let ctx =
        {
          ctx with
          value_map = IMap.fold IMap.add mod_ctx.value_map ctx.value_map;
          type_map = IMap.fold  IMap.add mod_ctx.type_map ctx.type_map;
        }
      in
      ctx

    | Pik_rename (sym_path, name) ->
      let path = path @ sym_path in
      let ident = Ident.Ident name.item in
      import_symbol ctx path ident

    | Pik_nested kinds ->
      let f ctx kind = resolve_import ctx path kind in
      List.fold_left f ctx kinds

(* ast stuff *)

let rec resolve_toplevel ctx tl =
  let qualnode ctx n = {n with item = Context.qualify ctx n.item} in
  let mk item = {tl with item} in
  match tl.item with
    | Ptl_value_def vdef ->
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
      ctx', [mk (Ptl_value_def vdef')]

    | Ptl_type_def tdef ->
      (* NOTE: types are always recursive now *)
      let td_name = qualnode ctx tdef.td_name in
      check_typedef_name td_name.span td_name.item;
      let ctx = Context.bind_type ctx tdef.td_name.item td_name.item in

      let ctx, type' = resolve_complex_type ctx tdef.td_type in
      let tdef' =
        {
          td_params = tdef.td_params;
          td_name;
          td_type = type';
        }
      in
      ctx, [mk (Ptl_type_def tdef')]

    | Ptl_extern_def edef ->
      let edef' =
        {
          ed_native_name = edef.ed_native_name;
          ed_name = qualnode ctx edef.ed_name;
          ed_type = resolve_type ctx edef.ed_type;
        }
      in
      let ctx = Context.bind_value ctx
        edef.ed_name.item
        edef'.ed_name.item
      in
      ctx, [mk (Ptl_extern_def edef')]

    | Ptl_module (n, ast) ->
      (* remove prefix '#' *)
      let name = unmark_string n.item in
      (* resolve module *)
      let subctx = {ctx with name = qualify ctx (Ident name)} in
      let subctx, ast = resolve_ast subctx ast in
      let ctx = add_subcontext_prefixed ctx subctx (Ident n.item) in
      ctx, ast

    | Ptl_import imp ->
      let ctx = resolve_import ctx imp.i_path imp.i_kind in
      ctx, []

and resolve_ast ctx = function
  | [] -> ctx, []
  | tl :: ast ->
    let ctx, tl = resolve_toplevel ctx tl in
    let ctx, ast = resolve_ast ctx ast in
    ctx, tl @ ast