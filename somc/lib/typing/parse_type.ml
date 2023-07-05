module Ident = Symbols.Ident
open Parse.Ast
open Types

module SMap = Map.Make(String)

let check_alias_exists env name span =
  try
    ignore (Env.lookup_alias env (Ident.Ident name))
  with Not_found ->
    let open Report.Error in
    let e = Type_error (Use_of_unbound ("type alias", name)) in
    Report.make_error e (Some span)
    |> Report.report
    (* Report.exit () *)

let rec parse ?(tvars=SMap.empty) env level =
  let go = parse ~tvars env in
  function
    | Pty_forall (ps, t) ->
      let f m var = SMap.add var (Types.new_var (level + 1)) m in
      let tvars = List.fold_left f tvars (nmapi ps) in
      parse ~tvars env (level + 1) t.item
      
    | Pty_grouping t -> go level t.item
    | Pty_wildcard -> Types.new_var level

    | Pty_variable n ->
      begin
        try SMap.find n.item tvars
        with Not_found ->
          let open Report.Error in
          let name = "'" ^ n.item in
          let e = Type_error (Use_of_unbound ("type variable", name)) in
          Report.make_error e (Some n.span)
          |> Report.report;
          TError
      end

    | Pty_effect t -> TEff (go level t.item)
    | Pty_function (a, r) -> TFun (go level a.item, go level r.item)
    | Pty_tuple ts -> TTup (List.map (go level) (nmapi ts))
    | Pty_construct (None, t) ->
      check_alias_exists env (Ident.to_string t.item) t.span;
      TName t.item

    | Pty_construct (Some a, t) ->
      check_alias_exists env (Ident.to_string t.item) t.span;
      TApp (go level a.item, TName t.item)

    | Pty_primitive s ->
      begin match s.item with
        | "Int" -> TPrim PInt
        | "Chr" -> TPrim PChar
        | "Bln" -> TPrim PBool
        | "Flt" -> TPrim PFloat
        | "Str" -> TPrim PString
        | "Nil" -> TPrim PNil
        | _ ->
          let open Report.Error in
          let e = Type_error (Use_of_invalid_primitive s.item) in
          Report.make_error e (Some s.span) |> Report.report;
          TError
      end

let parse_complex env params ident cmplxtyp =
  (* create tvars for params *)
  let f m var = SMap.add var (Types.new_var 1) m in
  let tvars = List.fold_left f SMap.empty (nmapi params) in

  (* wrap in constructed type *)
  let dest =
    let tvars' = SMap.bindings tvars |> List.split |> snd in
    let f acc t = TApp (t, acc) in
    List.fold_left f (TName ident) tvars' 
  in
  let env = Env.add_alias env ident dest in

  (* helper function *)
  let mkfty ts ret =
    let f acc t = TFun (t, acc) in
    List.fold_left f ret (List.rev ts)
  in

  let env, t = match cmplxtyp with
    | Pct_variant rows ->
      (* parses one row *)
      let parse_row (acc, env) (i, ts) =
        let ts' = List.map (parse ~tvars env 1) (nmapi ts) in
        let fty = mkfty ts' dest in
        acc @ [i.item, fty], Env.add_value env i.item fty
      in
      let rows', env = List.fold_left parse_row ([], env) rows in
      env, TVariant rows'

    | Pct_simple t -> env, parse ~tvars env 1 t.item
  in

  env, Unify.generalize 0 t
