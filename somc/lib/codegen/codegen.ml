module Opt = Opt
module Pass = Pass

(* open Lambda.Ir *)

type llmodule = Llvm.llmodule
let print_module m =
  Llvm.string_of_llmodule m
  |> print_endline

(* codegen stuff *)

let codegen_stmt _ctx = function
  (* | Stmt_definition _ -> failwith ""
  | Stmt_external (var, name) ->
    let fty = Llvm.var_arg_function_type 
    Llvm.declare_function  *)
  | _ -> failwith "TODO: codegen_stmt"

let codegen_program _program =
  let name = Filename.basename (!(Configs.Cli.args).file) in
  let ctx = Context.make name in
  (* let ctx = List.fold_left codegen_stmt ctx program in *)
  ctx.llmodule