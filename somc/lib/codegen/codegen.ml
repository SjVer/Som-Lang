open Lambda.Ir
open Context

module Opt = Opt
module Pass = Pass
module SMap = Map.Make(String)

type llmodule = Llvm.llmodule
let print_module m =
  Llvm.string_of_llmodule m
  |> print_endline

(* codegen stuff *)

let codegen_atom vals ctx = function
  | Atom_const c -> Value.llvalue_of_const ctx c
  | Atom_var (Var_local v)
  | Atom_var (Var_global v) -> SMap.find v vals
  | Atom_var (Var_tag _) -> invalid_arg "codegen_atom Atom_var Var_tag"
  | Atom_magic _ -> failwith "TODO: codegen_atom Atom_magic"

let rec codegen_expr vals ctx = function
  | Expr_let _ -> failwith "codegen_expr Expr_let"
  | Expr_lambda _ -> invalid_arg "codegen_expr Expr_lambda"
  | Expr_match _ -> failwith "codegen_expr Expr_match"

  | Expr_call _ -> failwith "codegen_expr Expr_call"
  
  | Expr_apply _ -> failwith "codegen_expr Expr_apply"

  | Expr_if (cond, texpr, eexpr) ->
    let curr_fn = Llvm.(block_parent (insertion_block ctx.builder)) in
    let tblock = Llvm.append_block ctx.context "tblock" curr_fn in
    let eblock = Llvm.append_block ctx.context "eblock" curr_fn in
    let pblock = Llvm.append_block ctx.context "pblock" curr_fn in

    let cond' = codegen_expr vals ctx cond in
    let _ = Llvm.build_cond_br cond' tblock eblock ctx.builder in

    Llvm.position_at_end tblock ctx.builder;
    let texpr' = codegen_expr vals ctx texpr in
    let tblock = Llvm.insertion_block ctx.builder in

    Llvm.position_at_end eblock ctx.builder;
    let eexpr' = codegen_expr vals ctx eexpr in
    let eblock = Llvm.insertion_block ctx.builder in

    Llvm.move_block_after eblock pblock;
    Llvm.position_at_end pblock ctx.builder;
    Llvm.build_phi [texpr', tblock; eexpr', eblock] "phi" ctx.builder

  | Expr_sequence _ -> failwith "codegen_expr Expr_sequence"
  | Expr_tuple _ -> failwith "codegen_expr Expr_tuple"
  | Expr_object _ -> failwith "codegen_expr Expr_object"
  | Expr_lazy _ -> failwith "codegen_expr Expr_lazy"
  | Expr_get _ -> failwith "codegen_expr Expr_get"
  | Expr_eval _ -> failwith "codegen_expr Expr_eval"
  | Expr_atom a -> codegen_atom vals ctx a
  | Expr_fail -> failwith "codegen_expr Expr_fail"

let codegen_stmt vals ctx = function
  | Stmt_definition (name, Expr_lambda (params, body)) ->
    let fty = Value.function_lltype ctx (List.length params) in
    let func = Llvm.define_function name fty ctx.llmodule in
    let entry = Llvm.entry_block func in
    Llvm.position_at_end entry ctx.builder;

    let body' =
      let f vals (p, v) =
        Llvm.set_value_name p v;
        SMap.add p v vals
      in
      let vals =
        Array.to_list (Llvm.params func)
        |> List.combine params
        |> List.fold_left f vals
      in
      codegen_expr vals ctx body
    in
    ignore (Llvm.build_ret body' ctx.builder);

    SMap.add name func vals

  | Stmt_definition (name, expr) ->
    let value = codegen_expr vals ctx expr in
    let glob = Llvm.define_global name value ctx.llmodule in
    SMap.add name glob vals

  | Stmt_external (name, native_name) ->
    let fty = Llvm.function_type (Value.value_lltype ctx) [||] in
    let glob = Llvm.declare_function native_name fty ctx.llmodule in
    SMap.add name glob vals

let codegen_program program =
  let name = Filename.basename !Configs.Cli.args.file in
  let ctx = Context.make name in

  let rec go vals ctx = function
    | [] -> ctx
    | stmt :: program ->
      let vals = codegen_stmt vals ctx stmt in
      go vals ctx program
  in
  (go SMap.empty ctx program).llmodule