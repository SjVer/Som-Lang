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

let extend env what key value =
  match what with
    | `Var -> {vars=SMap.add key value env.vars}
    (* {env with vars = SMap.add key value env.vars} *)

let lookup env `Var key = SMap.find key env.vars

let print env =
  print_endline "{";
  print_endline "\tvars: [";
  List.iter (fun (k, v) ->
    Printf.printf "\t\t%s: %s\n" k (Types.show_type v)
    ) (SMap.bindings env.vars);
  print_endline "\t]\n}";