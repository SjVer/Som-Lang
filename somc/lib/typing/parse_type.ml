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
    | TY_Grouping t -> go t.item
    | TY_Any -> Types.new_var l
    | TY_Forall _ -> failwith "Types.parse_type (forall)"
    | TY_Variable n -> get_tvar n
    | TY_Effect t -> TEff (go t.item)
    | TY_Function (a, r) -> TFun (go a.item, go r.item)
    | TY_Tuple ts -> 
      let map = List.map (fun t -> go t.item) in
      TTup (map ts)
    | TY_Primitive b -> begin
        let open Types in
        match b with
          | PT_Int (Some (s, w)) -> TPrim (PInt (s, w))
          | PT_Int None -> TVague (ref Int)
          | PT_Float (Some w) -> TPrim (PFloat w)
          | PT_Float None -> TVague (ref Float)
          | PT_Void -> TPrim PVoid
      end
    | TY_Construct (None, t) ->
      check_alias_exists env (Ident.to_string t.item) t.span;
      TName t.item
    | TY_Construct _ -> failwith "Types.parse_type (constr)"
    | TY_Variant _ -> failwith "Types.parse_type (variant)"
  in go