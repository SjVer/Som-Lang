open Lambda.Ir
open Context

module Opt = Opt
module Pass = Pass
module SMap = Map.Make(String)

type llmodule = Llvm.llmodule
let print_module m =
  Llvm.string_of_llmodule m
  |> print_endline

(* from https://github.com/ocaml/ocaml/blob/trunk/lambda/translcore.ml#L191-L194 *)
let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> invalid_arg "Codegen.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* codegen stuff *)

let codegen_atom vals ctx = function
  | Atom_const c -> Value.llvalue_of_const ctx c
  | Atom_var (Var_local v)
  | Atom_var (Var_global v) -> SMap.find v vals
  | Atom_var (Var_tag _) -> invalid_arg "codegen_atom Atom_var Var_tag"
  | Atom_magic _ -> failwith "TODO: codegen_atom Atom_magic"

let build_call_magic ctx magic args' =
  let args' = Array.of_list args' in
  let open Symbols.Magic in
  (* let build_f = match magic with
    | Magic_add -> Llvm.build_add args'.(0) args'.(1) "add"  
    | Magic_sub -> Llvm.build_sub args'.(0) args'.(1) "sub"  
    | Magic_mul -> Llvm.build_mul args'.(0) args'.(1) "mul"  
    | Magic_div -> Llvm.build_sdiv args'.(0) args'.(1) "div"  
    | Magic_rem -> Llvm.build_srem args'.(0) args'.(1) "rem"  
    | Magic_abs -> failwith "TODO: build_call_magic Magic_abs"
    | Magic_neg -> Llvm.build_neg args'.(0) "neg"
    | Magic_and -> Llvm.build_and args'.(0) args'.(1) "and"  
    | Magic_or  -> Llvm.build_or  args'.(0) args'.(1) "or"  
    | Magic_not -> Llvm.build_not args'.(0) "not"
    | Magic_eq  -> Llvm.build_icmp Llvm.Icmp.Eq args'.(0) args'.(1) "eq"
    | Magic_gt  -> Llvm.build_icmp Llvm.Icmp.Sgt args'.(0) args'.(1) "gt"
    | Magic_lt  -> Llvm.build_icmp Llvm.Icmp.Slt args'.(0) args'.(1) "lt"
    | Magic_neq -> Llvm.build_icmp Llvm.Icmp.Ne args'.(0) args'.(1) "neq"
    | Magic_gteq -> Llvm.build_icmp Llvm.Icmp.Sge args'.(0) args'.(1) "gteq"
    | Magic_lteq -> Llvm.build_icmp Llvm.Icmp.Sle args'.(0) args'.(1) "lteq"
    | Magic_tageq ->
      let ts = [|Value.value_lltype ctx; Llvm.i8_type ctx.context|] in
      let f = Value.get_ext_func ctx "_som_tag_is" ts in
      Llvm.build_call f args' "tageq"
  in
  build_f ctx.builder *)
  let name = "_som_magic_" ^ to_string magic in 
  let f = Value.get_ext_func ctx name (Array.map Llvm.type_of args') in
  Llvm.build_call f args' (to_string magic) ctx.builder 

let rec codegen_expr vals ctx = function
  | Expr_let _ -> failwith "codegen_expr Expr_let"
  | Expr_lambda _ -> invalid_arg "codegen_expr Expr_lambda"
  | Expr_match _ -> failwith "codegen_expr Expr_match"

  | Expr_call (Atom_magic m, args) ->
    let arity = Symbols.Magic.arity m in
    if List.length args <> arity then 
      failwith "Codegen call magic invalid arity";
    let args' = List.map (codegen_atom vals ctx) args in
    build_call_magic ctx m args'

  | Expr_call (f, args) ->
    let f' = codegen_atom vals ctx f in
    let args' = List.map (codegen_atom vals ctx) args in
    Llvm.build_call f' (Array.of_list args') "call" ctx.builder
  
  | Expr_apply (f, args) ->
    let f' = codegen_expr vals ctx f in
    (* TODO: actually make thunk *)
    let args' = List.map (codegen_atom vals ctx) args in
    Llvm.build_call f' (Array.of_list args') "thunk" ctx.builder

  | Expr_if (cond, texpr, eexpr) ->
    let curr_fn = Llvm.(block_parent (insertion_block ctx.builder)) in
    let tblock = Llvm.append_block ctx.context "tblock" curr_fn in
    let eblock = Llvm.append_block ctx.context "eblock" curr_fn in
    let pblock = Llvm.append_block ctx.context "pblock" curr_fn in

    let cond' = codegen_expr vals ctx cond in
    let _ = Llvm.build_cond_br cond' tblock eblock ctx.builder in

    Llvm.position_at_end tblock ctx.builder;
    let texpr' = codegen_expr vals ctx texpr in
    ignore (Llvm.build_br pblock ctx.builder);
    let tblock = Llvm.insertion_block ctx.builder in

    Llvm.position_at_end eblock ctx.builder;
    let eexpr' = codegen_expr vals ctx eexpr in
    ignore (Llvm.build_br pblock ctx.builder);
    let eblock = Llvm.insertion_block ctx.builder in

    Llvm.move_block_after eblock pblock;
    Llvm.position_at_end pblock ctx.builder;
    Llvm.build_phi [texpr', tblock; eexpr', eblock] "phi" ctx.builder

  | Expr_sequence (e1, e2) ->
    ignore (codegen_expr vals ctx e1);
    codegen_expr vals ctx e2

  | Expr_tuple _ -> failwith "codegen_expr Expr_tuple"
  | Expr_object _ -> failwith "codegen_expr Expr_object"
  | Expr_lazy _ -> failwith "codegen_expr Expr_lazy"
  | Expr_get _ -> failwith "codegen_expr Expr_get"
  | Expr_eval _ -> failwith "codegen_expr Expr_eval"
  | Expr_atom a -> codegen_atom vals ctx a
  
  | Expr_fail ->
    let f = Value.get_ext_func ctx "_som_fail" [||] in
    Llvm.build_call f [||] "unreachable" ctx.builder

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
    let fty = Value.function_lltype ctx 0 in
    let func = Llvm.define_function name fty ctx.llmodule in
    let entry = Llvm.entry_block func in
    Llvm.position_at_end entry ctx.builder;

    let value = codegen_expr vals ctx expr in
    ignore (Llvm.build_ret value ctx.builder);
    (* let glob = Llvm.define_global name value ctx.llmodule in *)
    
    SMap.add name func vals

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
