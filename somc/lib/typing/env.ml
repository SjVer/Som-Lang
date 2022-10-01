module SMap = Map.Make(String)

type t =
  {
    vars: Types.t SMap.t;
    (* classes: string list SMap.t; *)
  }

let empty =
  {
    vars=SMap.empty;
    (* classes=SMap.empty; *)
  }

let extend_var env name typ =
  {vars=SMap.add name typ env.vars}
  (* {env with vars = SMap.add key value env.vars} *)

let lookup_var env name = SMap.find name env.vars

let print env =
  print_endline "{";
  print_endline "\tvars: [";
  List.iter (fun (k, v) ->
    Printf.printf "\t\t%s: %s\n" k (Types.show_type v)
    ) (SMap.bindings env.vars);
  print_endline "\t]\n}";