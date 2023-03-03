open Parse.Ast
module IMap = Map.Make(Ident)

(* context stuff *)

type t =
  {
    (* path of module *)
    name: Ident.t;
    (* present submodules *)
    subcontexts: Ident.t list;
    (* maps of local to qualified identifiers *)
    value_map: Ident.t IMap.t;
    type_map: Ident.t IMap.t;
  }

let print ctx =
  print_endline ("======== " ^ (Ident.to_string ctx.name) ^ " ========");
  print_endline "Subcontexts:";
  List.iter
    (fun i -> print_endline ("\t" ^ Ident.to_string i))
    ctx.subcontexts;
  if ctx.subcontexts = [] then print_endline "\t<none>";
  print_newline ();

  let f k v = Printf.printf "\t%s -> %s\n"
    (Ident.to_string k)
    (Ident.to_string v)
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

let lookup_qual_value_ident ctx ident = IMap.find ident ctx.value_map
let lookup_qual_type_ident ctx ident = IMap.find ident ctx.type_map

(* let check_subcontext ctx ident =
  List.exists ((=) ident) ctx.subcontexts *)

(* adding *)

let add_local_value ctx vdef =
  let ident = vdef.vd_name.item in
  let qual_ident = qualify ctx ident in
  let value_map = IMap.add ident qual_ident ctx.value_map in
  {ctx with value_map}

let add_local_type ctx tdef =
  let ident = tdef.td_name.item in
  let qual_ident = qualify ctx ident in
  let type_map = IMap.add ident qual_ident ctx.type_map in
  {ctx with type_map}
  
let bind_qual_value_ident ctx ident qual =
  {ctx with value_map = IMap.add ident qual ctx.value_map}
let bind_qual_type_ident ctx ident qual =
  {ctx with type_map = IMap.add ident qual ctx.type_map}

(* let add_subcontext ctx ident =
  {ctx with subcontexts = ctx.subcontexts @ [ident]} *)

(* ugly shit *)

let add_subcontext_prefixed ctx subctx prefix =
  let f k e m = IMap.add (Ident.prepend (Ident prefix) k) e m in
  {
    ctx with
    subcontexts = ctx.subcontexts @ subctx.subcontexts @ [subctx.name];
    value_map = IMap.fold f subctx.value_map ctx.value_map;
    type_map = IMap.fold f subctx.type_map ctx.type_map;
  }

(* only changes the bindings *)
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