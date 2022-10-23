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
  let rec go = function
    | TY_Grouping t -> go t.item
    | TY_Any -> Types.new_var l
    | TY_Variable n -> get_tvar n
    | TY_Effect (Some t) -> TEff (go t.item)
    | TY_Effect None -> TEff TNever
    | TY_Function (a, r) -> TFun (go a.item, go r.item)
    | TY_Tuple ts -> 
      let map = List.map (fun t -> go t.item) in
      TTup (map ts)
    | TY_Builtin b -> begin
        let open Types in
        let p = match b with
          | BT_Int (s, w) -> PInt (s, w)
          | BT_Float w -> PFloat w
          | BT_Void -> PVoid
        in
        TPrim p
      end
    | TY_Construct (None, t) ->
      let path = Path.from_ident t.item in
      Env.check_alias_exists env (Path.to_string path) t.span;
      TName path
    | TY_Construct _ -> failwith "Types.parse_type (constr)"
    | TY_Variant _ -> failwith "Types.parse_type (variant)"
  in go