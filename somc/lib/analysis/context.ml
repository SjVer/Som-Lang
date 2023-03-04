open Parse.Ast
module IMap = Map.Make(Ident)

let next_id =
  let i = ref 0 in
  fun () -> incr i; !i

(* context stuff *)

type t =
  {
    (* path of module *)
    name: Ident.t;
    (* present submodules *)
    subcontexts: Ident.t list;
    (* maps of local to qualified identifiers *)
    value_map: (Ident.t * int) IMap.t;
    type_map: (Ident.t * int) IMap.t;
  }

let print ctx =
  print_endline ("======== " ^ (Ident.to_string ctx.name) ^ " ========");
  print_endline "Subcontexts:";
  List.iter
    (fun i -> print_endline ("\t" ^ Ident.to_string i))
    ctx.subcontexts;
  if ctx.subcontexts = [] then print_endline "\t<none>";
  print_newline ();

  let f k (q, i) = Printf.printf "\t%s -> %s (#%d)\n"
    (Ident.to_string k)
    (Ident.to_string q)
    i
  in

  print_endline "Value bindings: ";
  if IMap.is_empty ctx.value_map then print_endline "\t<none>"
  else IMap.iter f ctx.value_map;
  print_newline ();

  print_endline "Type bindings: ";
  if IMap.is_empty ctx.type_map then print_endline "\t<none>"
  else IMap.iter f ctx.type_map;
  print_newline ();

  print_endline "=============================="

let qualify ctx ident = Ident.append ctx.name ident

(* initializers *)

let empty name =
  {
    name;
    subcontexts = [];
    value_map = IMap.empty;
    type_map = IMap.empty;
  }

(* finding *)

let lookup_qual_value_ident ctx ident =
  IMap.find ident ctx.value_map |> fst

let lookup_qual_type_ident ctx ident =
  IMap.find ident ctx.type_map |> fst

(* let check_subcontext ctx ident =
  List.exists ((=) ident) ctx.subcontexts *)

(* adding *)

let bind_value ctx ident vdef =
  let entry = vdef.vd_name.item, next_id () in
  let value_map = IMap.add ident entry ctx.value_map in
  {ctx with value_map}

let bind_type ctx ident tdef =
  let entry = tdef.td_name.item, next_id () in
  let type_map = IMap.add ident entry ctx.type_map in
  {ctx with type_map}
  
let bind_qual_value_ident ctx ident qual =
  let entry = qual, next_id () in
  {ctx with value_map = IMap.add ident entry ctx.value_map}

let bind_qual_type_ident ctx ident qual =
  let entry = qual, next_id () in
  {ctx with type_map = IMap.add ident entry ctx.type_map}

(* let add_subcontext ctx ident =
  {ctx with subcontexts = ctx.subcontexts @ [ident]} *)

(* ugly shit *)

let prefix_bindings ctx prefix =
  let f k e m = IMap.add (Ident.prepend prefix k) e m in
  {
    ctx with
    value_map = IMap.fold f ctx.value_map IMap.empty;
    type_map = IMap.fold f ctx.type_map IMap.empty;
  }

let add_subcontext_prefixed ctx subctx prefix =
  let value_ids, type_ids =
    let f map =
      IMap.bindings map
      |> List.split |> snd
      |> List.split |> snd
    in
    f ctx.value_map, f ctx.type_map
  in
  let f ids k (q, i) m =
    (* skip if the binding already exists *)
    if List.exists ((=) i) ids then m
    else IMap.add (Ident.prepend (Ident prefix) k) (q, i) m
  in
  {
    ctx with
    subcontexts = ctx.subcontexts @ subctx.subcontexts @ [subctx.name];
    value_map = IMap.fold (f value_ids) subctx.value_map ctx.value_map;
    type_map = IMap.fold (f type_ids) subctx.type_map ctx.type_map;
  }

let extract_subcontext ctx prefix =
  let f m =
    let filter k _ = Ident.has_prefix k prefix
    and fold k e m = IMap.add (Ident.remove_prefix k prefix) e m in
    IMap.fold fold (IMap.filter filter m) IMap.empty
  in
  let filter_map i =
    if not (Ident.has_prefix i prefix) then Some i
    else try Some (Ident.remove_prefix i prefix)
    with Ident.Empty_ident -> None
  in
  {
    name = Ident (Ident.last prefix);
    subcontexts = List.filter_map filter_map ctx.subcontexts;
    value_map = f ctx.value_map;
    type_map = f ctx.type_map;
  }
