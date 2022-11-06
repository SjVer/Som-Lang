module SMap = Map.Make(String)

type t =
  {
    mutable symbols: Types.t SMap.t;
    mutable aliases: Types.t SMap.t;
    mutable sections: t SMap.t;
  }

let empty () =
  {
    symbols = SMap.empty;
    aliases = SMap.empty;
    sections = SMap.empty;
  }

let copy env =
  {
    symbols = env.symbols;
    aliases = env.aliases;
    sections = env.sections;
  }

(* returns an env that contains all the bindings
   that newenv contains but oldenv doesn't contain *)
let diff oldenv newenv =
  let f m n _ = not (Option.is_some (SMap.find_opt n m)) in
  let filter nm om = SMap.filter (f om) nm in
  {
    symbols = filter newenv.symbols oldenv.symbols;
    aliases = filter newenv.aliases oldenv.aliases;
    sections = filter newenv.sections oldenv.sections;
  }

let rec at_end_of_path e = function
  | 

let add_symbol  e n t = e.symbols <- SMap.add n t e.symbols
let add_alias   e n t =
  let rec go env = function
    | [] -> assert false
    | [n] -> env.aliases <- SMap.add n t env.aliases
    | n :: ns ->
      let env' = match SMap.find_opt n env.sections with
        | Some env -> env
        | None ->
          let env = empty () in
          e.sections <- SMap.add n env e.sections;
          env
      in
      go env' ns
  in go e (Path.to_list n)
  (* e.aliases <- SMap.add n t e.aliases *)

let add_section e n s = e.sections <- SMap.add n s e.sections

let get_symbol  e n = SMap.find n e.symbols
let get_alias   e n = SMap.find n e.aliases
let get_section e n = SMap.find n e.sections


let get_w_path env path span =
  let cant_use n =
    if n.[0] = '_' then
      let e = Report.Error.Cannot_private ("use", n) in
      Report.make_error (Type_error e) (Some span)
      |> Report.report;
      true
    else false
  in
  let rec go env first = function
    | [n] ->
      if not first && cant_use n then Types.TError
      else get_symbol env n
    | n :: ns ->
      let sect = get_section env n in
      if not first && cant_use n then Types.TError
      else go sect false ns
    | [] -> assert false
  in
  go env true (Path.to_list path)

let externals : Types.t SMap.t ref = ref SMap.empty


let show chan env =
  let w i s =
    if i > 20 then failwith "Env.show";
    let i' = String.make (i * 2) ' ' in
    Printf.fprintf chan "%s%s\n" i' s
  in
  let go_tmap i map =
    SMap.iter (fun k t ->
      w i ("\"" ^ k ^ "\": " ^ Types.show t true)
    ) map
  in
  let go_env i env = 
    w i "{";

    w (i+1) "symbols: {";
    go_tmap (i+2) env.symbols;
    w (i+1) "};";
    
    w (i+1) "aliases: {";
    go_tmap (i+2) env.aliases;
    w (i+1) "};";
    
    w (i+1) "sections: {";
    SMap.iter (fun n _ ->
      w (i+2) ("\"" ^ n ^ "\": ");
      (* go_env (i+3) e *) 
    ) env.sections;
    w (i+1) "};";

    w (i) "}";
  in
  go_env 0 env