module Ident = Symbols.Ident

open Parse.Ast
open Types

let check_alias_exists env name span =
  try
    ignore (Env.lookup_alias env (Ident.Ident name))
  with Not_found ->
    let open Report.Error in
    let what =
      if String.get name 0 = '\'' then "type variable"
      else "type alias"
    in
    let e = Type_error (Use_of_unbound (what, name)) in
    Report.make_error e (Some span)
    |> Report.report;
    Report.exit ()

let rec parse env level =
  let tvars = Hashtbl.create 10 in
  let get_tvar name =
    match Hashtbl.find_opt tvars name with
      | Some var -> var
      | None ->
        let var = Types.new_var level in
        Hashtbl.add tvars name var;
        var
  in function
    | TYGrouping t -> parse env level t.item
    | TYAny -> Types.new_var level
    | TYForall (ps, t) ->
      let add p =
        let t = Types.new_var (level + 1) in
        Hashtbl.add tvars p.item t
      in
      List.iter add ps;
      let t' = parse env (level + 1) t.item in
      List.iter (fun p -> Hashtbl.remove tvars p.item) ps;
      t'
    | TYVariable n -> get_tvar n.item
    | TYEffect t -> TEff (parse env level t.item)
    | TYFunction (a, r) ->
      TFun (parse env level a.item, parse env level r.item)
    | TYTuple ts -> 
      let map = List.map (fun t -> parse env level t.item) in
      TTup (map ts)
    | TYPrimitive b -> begin
        let open Types in
        match b with
          | PTInt (Some (s, w)) -> TPrim (PInt (s, w))
          | PTInt None -> TVague (ref VGInt)
          | PTFloat (Some w) -> TPrim (PFloat w)
          | PTFloat None -> TVague (ref VGFloat)
          | PTVoid -> TPrim PVoid
      end
    | TYConstruct (None, t) ->
      check_alias_exists env (Ident.to_string t.item) t.span;
      TName t.item
    | TYConstruct _ -> failwith "Types.parse_type (constr)"

let parse_complex env params dest cmplxtyp =
  (* used to pass on the [params] to [parse] *)
  let mk_fake_forall t = TYForall (params, t) in

  let env, t = match cmplxtyp with
    | CTVariant rows ->
      let mkfty acc t = TFun (t, acc) in
      let parse_row (acc, env) (i, ts) =
        let ts = List.map mk_fake_forall ts in
        let ts' = List.map (parse env 1) ts in
        let t = List.fold_left mkfty dest ts' in
        acc @ [i.item, t], Env.add_value env i.item t
      in
      let rows', env = List.fold_left parse_row ([], env) rows in
      env, TVariant rows'

    | CTSimple t -> env, parse env 1 t.item
  in

  env, Unify.generalize 0 t