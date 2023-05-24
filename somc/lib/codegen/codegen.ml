open Lambda.Ir
open Context

module Context = Context
module Emit = Emit
module Opt = Opt
module Pass = Pass
module SMap = Map.Make(String)

type llmodule = Llvm.llmodule
let print_module m =
  Llvm.string_of_llmodule m
  |> String.trim |> prerr_endline

(* from https://github.com/ocaml/ocaml/blob/trunk/lambda/translcore.ml#L191-L194 *)
let rec cut n l =
  if n = 0 then ([], l) else
  match l with
    | [] -> invalid_arg "codegen cut"
    | hd :: tl ->
      let (l1, l2) = cut (n - 1) tl in
      hd :: l1 , l2

let arity_of_func_ptr func =
  let typ = Llvm.type_of func in
  if Llvm.classify_type typ <> Llvm.TypeKind.Pointer then -1
  else
    let typ' = Llvm.element_type typ in
    if Llvm.classify_type typ' <> Llvm.TypeKind.Function then -1
    else Array.length (Llvm.param_types typ')

(* codegen stuff *)

let codegen_glob_eval glob ctx =
  (* let result_name = Llvm.value_name glob ^ ".result" in *)
  (* match Llvm.lookup_global result_name ctx.llmodule with *)
  (*   | Some result_glob -> *)
  (*     let curr_fn = Llvm.(block_parent (insertion_block ctx.builder)) in *)
  (*     let sblock = Llvm.insertion_block ctx.builder in *)
  (*     let eblock = Llvm.append_block ctx.context "eval" curr_fn in *)
  (*     let cblock = Llvm.append_block ctx.context "cont" curr_fn in *)

  (*     (* check if there's already a result *)  *)
  (*     let old_result = Llvm.build_load result_glob "old_result" ctx.builder in *)
  (*     let cond' = Llvm.build_is_not_null old_result "has_result" ctx.builder in *)
  (*     ignore (Llvm.build_cond_br cond' cblock eblock ctx.builder); *)

  (*     (* eval, store and return the result *) *)
  (*     Llvm.position_at_end eblock ctx.builder; *)
  (*     let new_result = Llvm.build_call glob [||] "new_result" ctx.builder in *)
  (*     ignore (Llvm.build_store new_result result_glob ctx.builder); *)
  (*     ignore (Llvm.build_br cblock ctx.builder); *)

  (*     Llvm.position_at_end cblock ctx.builder; *)
  (*     Llvm.build_phi [old_result, sblock; new_result, eblock] "result" ctx.builder *)

  (*   | None -> glob *)
  Llvm.build_call glob [||] (Llvm.value_name glob) ctx.builder

let codegen_atom vals ctx = function
  | Atom_const c -> Value.llvalue_of_const ctx c
  | Atom_var (Var_local v) -> SMap.find v vals
  | Atom_var (Var_global v) ->
    (* assume we need to call non-function globals *)
    let glob = SMap.find v vals in
    codegen_glob_eval glob ctx
  | Atom_var (Var_tag _) -> invalid_arg "codegen_atom var_tag"
  | Atom_prim _ -> failwith "TODO: codegen_atom Atom_prim"

let build_call_primitive ctx prim args' =
  let args' = Array.of_list args' in

  let one = Llvm.const_int (Llvm.i32_type ctx.context) 1 in
  let box v = Llvm.build_shl v one "box" ctx.builder in 
  let unbox v = Llvm.build_ashr v one "unbox" ctx.builder in
  let w2args f = f (unbox args'.(0)) (unbox args'.(1)) in
  
  (* TODO: the args are not unboxed, like, ever *)
  let open Symbols.Primitive in
  let build_f = match prim with
    | Prim_add_int
    | Prim_add_char  -> w2args Llvm.build_add
    | Prim_add_float -> w2args Llvm.build_add
    | Prim_add_string ->
      let sty = Value.value_lltype ctx in
      let f = Value.get_ext_func ctx "som_prim_add_string" [|sty; sty|] in
      Llvm.build_call f [|args'.(0)|]
    | Prim_sub_int
    | Prim_sub_char  -> w2args Llvm.build_sub
    | Prim_sub_float -> w2args Llvm.build_fsub
    | Prim_mul_int
    | Prim_mul_char  -> w2args Llvm.build_mul
    | Prim_mul_float -> w2args Llvm.build_fmul
    | Prim_div_int   -> w2args Llvm.build_sdiv
    | Prim_div_char  -> w2args Llvm.build_udiv
    | Prim_div_float -> w2args Llvm.build_fdiv
    | Prim_rem_int   -> w2args Llvm.build_srem 
    | Prim_rem_float -> w2args Llvm.build_frem
    | Prim_abs_int   ->
      let ity = Llvm.integer_type ctx.context 32 in
      let f = Value.get_ext_func ctx "som_prim_abs_int" [|ity; ity|] in
      Llvm.build_call f [|args'.(0); args'.(1)|]
    | Prim_abs_float ->
      let fty = Llvm.double_type ctx.context in
      let f = Value.get_ext_func ctx "som_prim_abs_float" [|fty; fty|] in
      Llvm.build_call f [|args'.(0); args'.(1)|]
    | Prim_neg_int   -> Llvm.build_neg args'.(0)
    | Prim_neg_float -> Llvm.build_fneg args'.(0)
    | Prim_and       -> w2args Llvm.build_and
    | Prim_or        -> w2args Llvm.build_or
    | Prim_not       -> Llvm.build_not args'.(0)
    | Prim_eq        -> w2args (Llvm.build_icmp Llvm.Icmp.Eq)
    | Prim_eq_value ->
      let vty = Value.value_lltype ctx in
      let f = Value.get_ext_func ctx "som_prim_eq_value" [|vty; vty|] in
      Llvm.build_call f [|args'.(0); args'.(1)|]
    | Prim_gt_int   -> w2args (Llvm.build_icmp Llvm.Icmp.Sgt)
    | Prim_gt_float -> w2args (Llvm.build_fcmp Llvm.Fcmp.Ogt)
    | Prim_lt_int   -> w2args (Llvm.build_icmp Llvm.Icmp.Slt)
    | Prim_lt_float -> w2args (Llvm.build_fcmp Llvm.Fcmp.Olt)
    | Prim_neq      -> w2args (Llvm.build_icmp Llvm.Icmp.Ne)
    | Prim_neq_value ->
      let vty = Value.value_lltype ctx in
      let f = Value.get_ext_func ctx "som_prim_neq_value" [|vty; vty|] in
      Llvm.build_call f [|args'.(0); args'.(1)|]
    | Prim_gteq_int   -> w2args (Llvm.build_icmp Llvm.Icmp.Sge)
    | Prim_gteq_float -> w2args (Llvm.build_fcmp Llvm.Fcmp.Oge)
    | Prim_lteq_int   -> w2args (Llvm.build_icmp Llvm.Icmp.Sle)
    | Prim_lteq_float -> w2args (Llvm.build_fcmp Llvm.Fcmp.Ole)
    | Prim_tageq -> failwith "TODO: codegen_prim tageq"
    
    | _ -> failwith ("codegen_prim " ^ (to_string prim))
  in
  build_f (to_string prim) ctx.builder
  |> box

let rec codegen_expr vals ctx = function
  | Expr_let (v, value, expr) ->
    let value' = codegen_expr vals ctx value in
    let vals = SMap.add v value' vals in
    codegen_expr vals ctx expr

  | Expr_lambda _ -> invalid_arg "codegen_expr lambda"
  | Expr_match _ -> failwith "codegen_expr Expr_match"

  | Expr_call (Atom_prim p, args) ->
    let arity = Symbols.Primitive.arity p in
    if List.length args <> arity then 
      failwith "codegen_expr primitive call invalid";
    let args' = List.map (codegen_atom vals ctx) args in
    build_call_primitive ctx p args'

  | Expr_call (f, args) ->
    let f' = match f with
      | Atom_var (Var_global v) -> SMap.find v vals
      | atom -> codegen_atom vals ctx atom
    in
    let args' = List.map (codegen_atom vals ctx) args in
    Llvm.build_call f' (Array.of_list args') "call" ctx.builder
  
  | Expr_apply (f, args) ->
    let f' = match f with
      | Expr_atom (Atom_var (Var_global v)) -> SMap.find v vals
      | f -> codegen_expr vals ctx f
    in
    let args' = List.map (codegen_atom vals ctx) args in
    (* TODO: actually make the closure *)
    Llvm.build_call f' (Array.of_list args') "closure" ctx.builder

  | Expr_if (cond, texpr, eexpr) ->
    let curr_fn = Llvm.(block_parent (insertion_block ctx.builder)) in
    let tblock = Llvm.append_block ctx.context "then" curr_fn in
    let eblock = Llvm.append_block ctx.context "else" curr_fn in
    let pblock = Llvm.append_block ctx.context "cont" curr_fn in

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

  | Expr_tuple els ->
    let els' = List.map (codegen_atom vals ctx) els in
    Value.make_object ctx 253 els' 

  | Expr_object _ -> failwith "codegen_expr Expr_object"
  | Expr_lazy _ -> failwith "codegen_expr Expr_lazy"
  | Expr_get _ -> failwith "codegen_expr Expr_get"
  | Expr_eval _ -> failwith "codegen_expr Expr_eval"
  
  | Expr_atom a -> codegen_atom vals ctx a
  | Expr_fail ->
    let f = Value.get_ext_func ctx "som_fail_match" [||] in
    Llvm.build_call f [||] "unreachable" ctx.builder

let codegen_stmt vals ctx = function
  | Stmt_definition (name, Expr_lambda (params, body)) ->
    let fty = Value.function_lltype ctx (List.length params) in
    let func = Llvm.define_function name fty ctx.llmodule in
    let entry = Llvm.entry_block func in
    Llvm.position_at_end entry ctx.builder;

    let body' =
      let vals =
        let f vals (p, v) =
          Llvm.set_value_name p v;
          SMap.add p v vals
        in
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
   
    (* ignore (Llvm.define_global *)
    (*   (name ^ ".result") *)
    (*   (Llvm.const_null (Value.value_lltype ctx)) *)
    (*   ctx.llmodule); *)

    SMap.add name func vals

  | Stmt_external (name, native_name) ->
    let fty = Llvm.function_type (Value.value_lltype ctx) [||] in
    let glob = Llvm.declare_function native_name fty ctx.llmodule in
    SMap.add name glob vals

let codegen_entrypoint vals ctx =
  try
    let main_fn =
      let f n =
        match String.split_on_char '/' n with
          | n :: _ -> n = "main"
          | [] -> false
      in
      snd (SMap.find_last f vals)
    in
    Llvm.define_global "som_entrypoint" main_fn ctx.llmodule
    |> ignore

  with Not_found ->
    let open Report.Error in
    let e = Other_error (Other "no entrypoint defined") in
    Report.raise (Report.make_error e None)

let codegen_program program =
  Llvm.enable_pretty_stacktrace ();
  Llvm_all_backends.initialize ();

  let name = Filename.basename !Configs.Cli.args.file in
  let ctx = Context.make name in

  let rec go vals ctx = function
    | [] -> vals, ctx
    | stmt :: program ->
      let vals = codegen_stmt vals ctx stmt in
      go vals ctx program
  in
  let vals, ctx = go SMap.empty ctx program in
  codegen_entrypoint vals ctx;
  ctx

