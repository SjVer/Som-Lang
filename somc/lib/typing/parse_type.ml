module Ident = Symbols.Ident

let check_alias_exists env name span =
  try
    ignore (Env.lookup_alias env (Ident.Ident name))
  with Not_found ->
    let open Report.Error in
    let e = Type_error (Use_of_unbound ("type alias", name)) in
    Report.make_error e (Some span)
    |> Report.report;
    Report.exit ()

let parse env l =
  let gnames = Hashtbl.create 10 in
  let get_tvar name =
    match Hashtbl.find_opt gnames name with
      | Some var -> var
      | None ->
        let var = Types.new_var l in
        Hashtbl.add gnames name var;
        var
  in
  let open Parse.Ast in
  let open Types in
  let rec go = function
    | TYGrouping t -> go t.item
    | TYAny -> Types.new_var l
    | TYForall _ -> failwith "Types.parse_type (forall)"
    | TYVariable n -> get_tvar n
    | TYEffect t -> TEff (go t.item)
    | TYFunction (a, r) -> TFun (go a.item, go r.item)
    | TYTuple ts -> 
      let map = List.map (fun t -> go t.item) in
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
    | TYVariant _ -> failwith "Types.parse_type (variant)"
  in go