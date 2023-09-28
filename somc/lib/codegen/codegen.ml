open Lambda.Ir
open Context
open Value

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
  Llvm.build_call2 (value_lltype ctx) glob [||] (Llvm.value_name glob) ctx.builder

let codegen_atom vals ctx = function
  | Atom_const c -> llvalue_of_const ctx c
  | Atom_var (Var_local v) -> SMap.find v vals
  | Atom_var (Var_global v) ->
    (* assume we need to call non-function globals *)
    let glob = SMap.find v vals in
    codegen_glob_eval glob ctx
  | Atom_var (Var_tag _) -> invalid_arg "codegen_atom var_tag"
  | Atom_prim _ -> failwith "TODO: codegen_atom Atom_prim"

let cast_value_to_short_t ctx v t =
  let v' = Llvm.build_trunc 
    v (Llvm.i32_type ctx.context) 
    "trunc" ctx.builder
  in
  Llvm.build_bitcast v' t "cast" ctx.builder

let cast_short_t_to_value ctx v =
  let v' = Llvm.build_bitcast
    v (Llvm.i32_type ctx.context)
    "cast" ctx.builder
  in
  Llvm.build_zext 
    v' (value_lltype ctx)
    "zext" ctx.builder 

let build_call_primitive ctx prim args' =
  let name = Symbols.Primitive.to_string prim in
  let args' = Array.of_list args' in
  
  let rawop2 f =
    let r = f (unbox args'.(0) ctx) (unbox args'.(1) ctx) name ctx.builder in
    box r ctx
  in
  let chrop2 f =
    let t = Llvm.i8_type ctx.context in
    let arg1 = cast_value_to_short_t ctx args'.(0) t in
    let arg2 = cast_value_to_short_t ctx args'.(1) t in
    let result = f arg1 arg2 name ctx.builder in
    cast_short_t_to_value ctx result
  in
  let fltop2 name =
    let fname = "som_prim_" ^ name ^ "_float" in
    let t = value_lltype ctx in
    let f = get_ext_func ctx fname [|t; t|] in
    build_call f [|args'.(0); args'.(1)|] name ctx
  in

  let open Symbols.Primitive in
  match prim with
    | Prim_add_int   -> rawop2 Llvm.build_add
    | Prim_add_char  -> chrop2 Llvm.build_add
    | Prim_add_float -> fltop2 "add"
    | Prim_add_string ->
      let sty = value_lltype ctx in
      let f = get_ext_func ctx "som_prim_add_string" [|sty; sty|] in
      build_call f [|args'.(0)|] name ctx

    | Prim_sub_int   -> rawop2 Llvm.build_sub
    | Prim_sub_char  -> chrop2 Llvm.build_sub
    | Prim_sub_float -> rawop2 Llvm.build_fsub

    | Prim_mul_int   -> rawop2 Llvm.build_mul
    | Prim_mul_char  -> chrop2 Llvm.build_mul
    | Prim_mul_float -> fltop2 "mul"

    | Prim_div_int   -> rawop2 Llvm.build_sdiv
    | Prim_div_char  -> chrop2 Llvm.build_udiv
    | Prim_div_float -> fltop2 "div"

    | Prim_rem_int   -> rawop2 Llvm.build_srem 
    | Prim_rem_float -> fltop2 "rem"

    | Prim_abs_int   ->
      let ity = Llvm.integer_type ctx.context 32 in
      let f = get_ext_func ctx "som_prim_abs_int" [|ity; ity|] in
      build_call f [|args'.(0); args'.(1)|] name ctx
    | Prim_abs_float ->
      let fty = Llvm.double_type ctx.context in
      let f = get_ext_func ctx "som_prim_abs_float" [|fty; fty|] in
      build_call f [|args'.(0); args'.(1)|] name ctx

    | Prim_neg_int   -> Llvm.build_neg args'.(0) name ctx.builder
    | Prim_neg_float -> Llvm.build_fneg args'.(0) name ctx.builder

    | Prim_and       -> rawop2 Llvm.build_and
    | Prim_or        -> rawop2 Llvm.build_or
    | Prim_not       -> Llvm.build_not args'.(0) name ctx.builder

    | Prim_eq        -> rawop2 (Llvm.build_icmp Llvm.Icmp.Eq)
    | Prim_eq_value ->
      let vty = value_lltype ctx in
      let f = get_ext_func ctx "som_prim_eq_value" [|vty; vty|] in
      build_call f [|args'.(0); args'.(1)|] name ctx

    | Prim_gt_int   -> rawop2 (Llvm.build_icmp Llvm.Icmp.Sgt)
    | Prim_gt_float -> fltop2 "gt"
    | Prim_lt_int   -> rawop2 (Llvm.build_icmp Llvm.Icmp.Slt)
    | Prim_lt_float -> fltop2 "lt"

    | Prim_neq      -> rawop2 (Llvm.build_icmp Llvm.Icmp.Ne)
    | Prim_neq_value ->
      let vty = value_lltype ctx in
      let f = get_ext_func ctx "som_prim_neq_value" [|vty; vty|] in
      build_call f [|args'.(0); args'.(1)|] name ctx

    | Prim_gteq_int   -> rawop2 (Llvm.build_icmp Llvm.Icmp.Sge)
    | Prim_gteq_float -> fltop2 "gteq"
    | Prim_lteq_int   -> rawop2 (Llvm.build_icmp Llvm.Icmp.Sle)
    | Prim_lteq_float -> fltop2 "lteq"

    | Prim_tageq -> failwith "TODO: codegen_prim tageq"
    
    | _ -> failwith ("codegen_prim " ^ (to_string prim))

let rec codegen_expr vals ctx = function
  | Expr_let (v, value, expr) ->
    let value' = codegen_expr vals ctx value in
    let vals = SMap.add v value' vals in
    codegen_expr vals ctx expr

  | Expr_lambda _ -> failwith "codegen_expr lambda"
  | Expr_match _ -> failwith "codegen_expr Expr_match"

  | Expr_call (Atom_prim p, args) ->
    let arity = Symbols.Primitive.arity p in
    if List.length args <> arity then 
      failwith "codegen_expr primitive call invalid";
    let args' = List.map (codegen_expr vals ctx) args in
    build_call_primitive ctx p args'

  | Expr_call (f, args) ->
    let f' = match f with
      | Atom_var (Var_global v) -> SMap.find v vals
      | atom -> codegen_atom vals ctx atom
    in
    let args' = List.map (codegen_expr vals ctx) args in
    build_call f' (Array.of_list args') "call" ctx
  
  | Expr_apply (f, args) ->
    let f', arity = match f with
      | Expr_atom (Atom_var (Var_global v)) ->
        let f' = SMap.find v vals in
        f', Array.length (Llvm.params f')
      | f -> codegen_expr vals ctx f, 0
    in

    let f' = Llvm.build_bitcast
      f' (value_lltype ctx)
      (Llvm.value_name f') ctx.builder
    in
    let args' = List.map (codegen_expr vals ctx) args in

    (* call som_apply(&func, arity, argc, args...) *)
    let ity = Llvm.i32_type ctx.context in
    let func = get_ext_func ctx
      "som_make_closure" ~vararg:true
      [|value_lltype ctx; ity; ity|] 
    in
    let arity' = Llvm.const_int ity arity in
    let argc = Llvm.const_int ity (List.length args) in
    let args'' = Array.append [|f'; arity'; argc|] (Array.of_list args') in
    build_call func args'' "closure" ctx

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
    make_fields_obj ctx Tag.tuple els'
  | Expr_object (tag, els) ->
    let els' = List.map (codegen_atom vals ctx) els in
    make_fields_obj ctx tag els' 
  
  | Expr_lazy _ -> failwith "codegen_expr Expr_lazy"

  | Expr_get (expr, index) ->
    let expr' = codegen_atom vals ctx (Atom_var expr) in
    let offset = 8 + index * word_size ctx in
    let offset' = Llvm.const_int (Llvm.i64_type ctx.context) offset in
    Llvm.build_gep2 (value_lltype ctx) expr' [|offset'|] "get" ctx.builder
  
  | Expr_eval _ -> failwith "codegen_expr Expr_eval"
  
  | Expr_atom a -> codegen_atom vals ctx a
  | Expr_fail ->
    let f = get_ext_func ctx "som_fail_match" [||] in
    build_call f [||] "unreachable" ctx

let codegen_stmt vals ctx = function
  | Stmt_definition (name, expr) ->
    let fty = function_lltype ctx 0 in
    let func = Llvm.define_function name fty ctx.llmodule in

    let entry = Llvm.entry_block func in
    Llvm.position_at_end entry ctx.builder;
    
    let value = codegen_expr vals ctx expr in
    ignore (Llvm.build_ret value ctx.builder);
  
    (* ignore (Llvm.define_global *)
    (*   (name ^ ".result") *)
    (*   (Llvm.const_null (value_lltype ctx)) *)
    (*   ctx.llmodule); *)

    SMap.add name func vals

  | Stmt_function (name, params, body) ->
    let fty = function_lltype ctx (List.length params) in
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
    print_endline ("=== " ^ name ^ " : " ^ Llvm.string_of_lltype (Llvm.type_of body') ^ " = " ^ Llvm.string_of_llvalue body');
    ignore (Llvm.build_ret body' ctx.builder);

    SMap.add name func vals

  | Stmt_external (name, native_name, arity) ->
    let fty = function_lltype ctx arity in
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
      let name =
        SMap.bindings vals 
        |> List.split |> fst
        |> List.sort String.compare
        |> List.rev |> List.find f
      in
      SMap.find name vals
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

let optimize_module ctx =
  Pass.run_passes ctx.llmodule;
  ctx