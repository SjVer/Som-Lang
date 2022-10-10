module SMap = Map.Make(String)

type t =
  {
    vars: Types.t SMap.t;
    sects: t SMap.t;
    (* classes: string list SMap.t; *)
  }

let empty =
  {
    vars=SMap.empty;
    sects=SMap.empty;
  }

let extend_var env name typ =
  {env with vars=SMap.add name typ env.vars}
let extend_sect env name sect =
  {env with sects=SMap.add name sect env.sects}

let lookup_var env name = SMap.find name env.vars
let lookup_sect env name = SMap.find name env.sects

let lookup_w_path env path =
  let rec go env' = function
    | [n] -> lookup_var env' n
    | n :: ns -> go (lookup_sect env' n) ns
    | [] -> assert false
  in
  go env (Path.to_list path)