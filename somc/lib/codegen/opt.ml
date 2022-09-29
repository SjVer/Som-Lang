type opt_level =
  | On (** No optimization *)
  | O0 (** Almost no optimization *)
  | O1 (** Simple optimization *)
  | O2 (** Moderate optimization *)
  | O3 (** Maximum optimization *)
  | Os (** Code size optimization *)
  | Oz (** Severe code size optimization *)

let show_opt_level = function
  | On -> "On"
  | O0 -> "O0"
  | O1 -> "O1"
  | O2 -> "O2"
  | O3 -> "O3"
  | Os -> "Os"
  | Oz -> "Oz"

(** creates a passmgr_builder with the given opts *)
let get_passmgr_builder_with_opt_level level =
  let pm_bld = Llvm_passmgr_builder.create () in 
  let set_s = Llvm_passmgr_builder.set_size_level in
  let set_o = Llvm_passmgr_builder.set_opt_level in
  begin match level with
    | On -> ()
    | O0 -> set_o 0 pm_bld
    | O1 -> set_o 1 pm_bld
    | O2 -> set_o 2 pm_bld
    | O3 -> set_o 3 pm_bld
    | Os -> set_s 1 pm_bld
    | Oz -> set_s 2 pm_bld
  end;
  pm_bld

(** optimizes the given module. *)
let optimize mdl level =
  if level = On then () else

  let pm_bld = get_passmgr_builder_with_opt_level level in
  let pm = Llvm.PassManager.create () in
  Llvm_passmgr_builder.populate_module_pass_manager pm pm_bld;
  
  ignore (Llvm.PassManager.run_module mdl pm)