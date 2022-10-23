type t =
  {
    name: string;
    func: [`Module] Llvm.PassManager.t -> unit;
  }

let get_scalar_opts_pass =
  let open Llvm_scalar_opts in function
  | "aggressive-dce" -> Some add_aggressive_dce
  | "alignment-from-assumptions" -> Some add_alignment_from_assumptions
  | "basic-alias-analysis" -> Some add_basic_alias_analysis
  | "cfg-simplification" -> Some add_cfg_simplification
  | "correlated-value-propagation" -> Some add_correlated_value_propagation
  | "dce" -> Some add_dce
  | "dead-store-elimination" -> Some add_dead_store_elimination
  | "early-cse" -> Some add_early_cse
  | "gvn" -> Some add_gvn
  | "ind-var-simplification" -> Some add_ind_var_simplification
  | "instruction-combination" -> Some add_instruction_combination
  | "jump-threading" -> Some add_jump_threading
  | "lib-call-simplification" -> Some add_lib_call_simplification
  | "licm" -> Some add_licm
  | "loop-deletion" -> Some add_loop_deletion
  | "loop-idiom" -> Some add_loop_idiom
  | "loop-reroll" -> Some add_loop_reroll
  | "loop-rotation" -> Some add_loop_rotation
  | "loop-unroll" -> Some add_loop_unroll
  | "loop-unswitch" -> Some add_loop_unswitch
  | "lower-atomic" -> Some add_lower_atomic
  | "lower-expect-intrinsic" -> Some add_lower_expect_intrinsic
  | "lower-switch" -> Some add_lower_switch
  | "memcpy-opt" -> Some add_memcpy_opt
  | "memory-to-register-demotion" -> Some add_memory_to_register_demotion
  | "memory-to-register-promotion" -> Some add_memory_to_register_promotion
  | "merged-load-store-motion" -> Some add_merged_load_store_motion
  | "partially-inline-lib-calls" -> Some add_partially_inline_lib_calls
  | "reassociation" -> Some add_reassociation
  | "scalar-repl-aggregation" -> Some add_scalar_repl_aggregation
  | "scalar-repl-aggregation-ssa" -> Some add_scalar_repl_aggregation_ssa
  (* | "scalar-repl-aggregation-with-threshold" -> Some add_scalar_repl_aggregation_with_threshold  *)
  | "scalarizer" -> Some add_scalarizer
  | "sccp" -> Some add_sccp
  | "scoped-no-alias-alias-analysis" -> Some add_scoped_no_alias_alias_analysis
  | "tail-call-elimination" -> Some add_tail_call_elimination
  | "type-based-alias-analysis" -> Some add_type_based_alias_analysis
  | "unify-function-exit-nodes" -> Some add_unify_function_exit_nodes
  | "verifier" -> Some add_verifier
  | _ -> None

let get_vectorize_pass =
  let open Llvm_vectorize in function
    | "loop-vectorize" -> Some add_loop_vectorize
    | "slp-vectorize" -> Some add_slp_vectorize
    | _ -> None

let get_ipo_pass =
  let open Llvm_ipo in function
    | "argument-promotion" -> Some add_argument_promotion
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
    | "internalize" -> Some (fun x -> add_internalize x ~all_but_main:false)
    | "strip-dead-prototypes" -> Some add_strip_dead_prototypes
    | "strip-symbols" -> Some add_strip_symbols
    | _ -> None

(** gets a pass by name or raises [Not_found]
    if it doesn't exist *)
let get_pass name =
  let rec try_get = function
    | [] ->
      let open Report in
      let open Error in
      make_error (Other_error (Nonexistent_pass name)) None
      |> add_note "for a list of supported passes go to\n\
                  https://documentation.isnt.written.yet."
      |> report;
      Report.exit 1
    | fn :: fns -> match fn name with
      | Some pass -> pass
      | None -> try_get fns
  in
  let func = try_get [
    get_scalar_opts_pass;
    get_vectorize_pass;
    get_ipo_pass;
  ]
  in {name; func}

let add_pass pm pass =
  Report.make_note ("registering LLVM pass " ^ pass.name) None
  |> Report.report;
  pass.func pm