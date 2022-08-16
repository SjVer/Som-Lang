type opt_level =
  | O0 (** No optimization *)
  | O1 (** Simple optimization *)
  | O2 (** Moderate optimization *)
  | O3 (** Maximum optimization *)
  | Os (** Code size optimization *)
  | Oz (** Severe code size optimization *)

let show_opt_level = function
  | O0 -> "O0"
  | O1 -> "O1"
  | O2 -> "O2"
  | O3 -> "O3"
  | Os -> "Os"
  | Oz -> "Oz"

(** applies the given optimization level to [b] *)
let apply_opt b =
  let set_s = Llvm_passmgr_builder.set_size_level in
  let set_o = Llvm_passmgr_builder.set_opt_level in
  function
    | O0 -> set_o 0 b
    | O1 -> set_o 1 b
    | O2 -> set_o 2 b
    | O3 -> set_o 3 b
    | Os -> set_s 1 b
    | Oz -> set_s 2 b