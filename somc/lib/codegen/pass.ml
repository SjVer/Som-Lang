module C = Configs.Cli

let get_scalar_opts_pass =
  let open Llvm_scalar_opts in function
    | "add-aggressive-dce" -> Some add_aggressive_dce
    | "add-dce" -> Some add_dce
    | "add-alignment-from-assumptions" -> Some add_alignment_from_assumptions
    | "add-cfg-simplification" -> Some add_cfg_simplification
    | "add-dead-store-elimination" -> Some add_dead_store_elimination
    | "add-scalarizer" -> Some add_scalarizer
    | "add-merged-load-store-motion" -> Some add_merged_load_store_motion
    | "add-gvn" -> Some add_gvn
    | "add-ind-var-simplification" -> Some add_ind_var_simplification
    | "add-instruction-combination" -> Some add_instruction_combination
    | "add-jump-threading" -> Some add_jump_threading
    | "add-licm" -> Some add_licm
    | "add-loop-deletion" -> Some add_loop_deletion
    | "add-loop-idiom" -> Some add_loop_idiom
    | "add-loop-rotation" -> Some add_loop_rotation
    | "add-loop-reroll" -> Some add_loop_reroll
    | "add-loop-unroll" -> Some add_loop_unroll
    | "add-memcpy-opt" -> Some add_memcpy_opt
    | "add-partially-inline-lib-calls" -> Some add_partially_inline_lib_calls
    | "add-lower-atomic" -> Some add_lower_atomic
    | "add-lower-switch" -> Some add_lower_switch
    | "add-memory-to-register-promotion" -> Some add_memory_to_register_promotion
    | "add-reassociation" -> Some add_reassociation
    | "add-sccp" -> Some add_sccp
    | "add-scalar-repl-aggregation" -> Some add_scalar_repl_aggregation
    | "add-scalar-repl-aggregation-ssa" -> Some add_scalar_repl_aggregation_ssa
    (* | "add-scalar-repl-aggregation-with-threshold" -> Some add_scalar_repl_aggregation_with_threshold *)
    | "add-lib-call-simplification" -> Some add_lib_call_simplification
    | "add-tail-call-elimination" -> Some add_tail_call_elimination
    | "add-memory-to-register-demotion" -> Some add_memory_to_register_demotion
    | "add-verifier" -> Some add_verifier
    | "add-correlated-value-propagation" -> Some add_correlated_value_propagation
    | "add-early-cse" -> Some add_early_cse
    | "add-lower-expect-intrinsic" -> Some add_lower_expect_intrinsic
    | "add-lower-constant-intrinsics" -> Some add_lower_constant_intrinsics
    | "add-type-based-alias-analysis" -> Some add_type_based_alias_analysis
    | "add-scoped-no-alias-alias-analysis" -> Some add_scoped_no_alias_alias_analysis
    | "add-basic-alias-analysis" -> Some add_basic_alias_analysis
    | "add-unify-function-exit-nodes" -> Some add_unify_function_exit_nodes

    | "verifier" -> Some add_verifier
    | _ -> None

let get_vectorize_pass =
  let open Llvm_vectorize in function
    | "loop-vectorize" -> Some add_loop_vectorize
    | "slp-vectorize" -> Some add_slp_vectorize
    | _ -> None

let get_ipo_pass =
  let open Llvm_ipo in function
    | "constant-merge" -> Some add_constant_merge
    | "merge-functions" -> Some add_merge_functions
    | "dead_arg-elimination" -> Some add_dead_arg_elimination
    | "function-attrs" -> Some add_function_attrs
    | "function-inlining" -> Some add_function_inlining
    | "always-inliner" -> Some add_always_inliner
    | "global-dce" -> Some add_global_dce
    | "global-optimizer" -> Some add_global_optimizer
    | "prune-eh" -> Some add_prune_eh
    | "ipsccp" -> Some add_ipsccp
    | "internalize" -> Some (fun x -> add_internalize x ~all_but_main:true)
    | "strip-dead-prototypes" -> Some add_strip_dead_prototypes
    | "strip-symbols" -> Some add_strip_symbols
    | _ -> None

let get_pass name =
  let rec try_get = function
    | [] ->
      let open Report in
      let open Error in
      make_error (Other_error (Nonexistent_pass name)) None
      |> add_note "for a list of supported passes check your\n\
                   installed LLVM version's documentation."
      |> report;
      Report.exit ()
    | fn :: fns -> match fn name with
      | Some pass -> pass
      | None -> try_get fns
  in
  try_get [
    get_scalar_opts_pass;
    get_vectorize_pass;
    get_ipo_pass;
  ]

let run_passes llmodule =
  if !C.args.verbose then 
    Report.report_note "validating LLVM IR";
  Llvm_analysis.assert_valid_module llmodule;

  if !C.args.opt_level <> `On then
    let pm = Llvm.PassManager.create () in
    let pmbld = Llvm_passmgr_builder.create () in

    (* add default passes *)
    begin match !C.args.opt_level with
      | `On -> failwith "run_passes On"
      | `O0 -> Llvm_passmgr_builder.set_opt_level 0
      | `O1 -> Llvm_passmgr_builder.set_opt_level 1
      | `O2 -> Llvm_passmgr_builder.set_opt_level 2
      | `O3 -> Llvm_passmgr_builder.set_opt_level 3
      | `Os -> Llvm_passmgr_builder.set_size_level 1
      | `Oz -> Llvm_passmgr_builder.set_size_level 2
    end pmbld;

    (* add user's extra passes *)
    let add pass =
      let func = get_pass pass in
      Report.make_note ("added LLVM pass: " ^ pass) None
      |> Report.report;
      func pm
    in
    List.iter add !C.args.passes;

    (* run the bitch *)
    if !C.args.verbose then
      Report.report_note "running LLVM passes";

    (* Llvm_passmgr_builder.populate_lto_pass_manager
      pm ~internalize:true ~run_inliner:true pmbld; *)
    Llvm_passmgr_builder.use_inliner_with_threshold 225 pmbld;
    Llvm_passmgr_builder.populate_module_pass_manager pm pmbld;

    let iter = ref 0 in
    while
      Llvm.PassManager.run_module llmodule pm
      && !iter <= 1000
    do incr iter done
  else ()

