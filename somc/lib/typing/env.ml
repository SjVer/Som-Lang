module Ident = Symbols.Ident
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

(* add functions *)

let add_symbol e n t = e.symbols <- SMap.add n t e.symbols

let add_alias e n t = e.aliases <- SMap.add n t e.aliases

let add_section e n s = e.sections <- SMap.add n s e.sections

(* get functions *)

let at_end_of_path env path span =
  let check_can_use n =
    if n.[0] = '_' then
      let e = Report.Error.Cannot_private ("use", n) in
      Report.make_error (Type_error e) (Some span)
      |> Report.report
  in
  let rec go env first = function
    | [n] ->
      if not first then check_can_use n;
      env, n
    | n :: ns ->
      let sect = SMap.find n env.sections in
      if not first then check_can_use n;
      go sect false ns
    | [] -> assert false
  in
  go env true (Ident.to_list path)

let get_section env n = SMap.find n env.sections

let get_symbol env p span =
  let env', n = at_end_of_path env p span in
  SMap.find n env'.symbols

let get_alias env p s =
  let env', n = at_end_of_path env p s in
  SMap.find n env'.aliases

(* misc *)

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