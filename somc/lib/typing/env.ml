module SMap = Map.Make(String)

type t =
  {
    symbols: Types.t SMap.t; (* types of variables *)
    aliases: Types.t SMap.t; (* type aliases *)
    sections: t SMap.t;
    (* classes: string list SMap.t; *)
  }

let empty =
  {
    symbols=SMap.empty;
    aliases=SMap.empty;
    sections=SMap.empty;
  }

(* returns an env that contains all the bindings
   that newenv contains but oldenv doesn't contain *)
let diff oldenv newenv =
  let f m n _ = not (Option.is_some (SMap.find_opt n m)) in
  let filter nm om = SMap.filter (f om) nm in
  {
    symbols=filter newenv.symbols oldenv.symbols;
    aliases=filter newenv.aliases oldenv.aliases;
    sections=filter newenv.sections oldenv.sections;
  }

let add_symbol  e n t = {e with symbols=SMap.add n t e.symbols}
let add_alias   e n t = {e with aliases=SMap.add n t e.aliases}
let add_section e n s = {e with sections=SMap.add n s e.sections}

let get_symbol  e n = SMap.find n e.symbols
let get_alias   e n = SMap.find n e.aliases
let get_section e n = SMap.find n e.sections

let get_w_path env path =
  let rec go env' = function
    | [n] -> get_symbol env' n
    | n :: ns -> go (get_section env' n) ns
    | [] -> assert false
  in
  go env (Path.to_list path)

let externals : Types.t SMap.t ref = ref SMap.empty