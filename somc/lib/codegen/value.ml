open Context

let word_is_32bit = Sys.word_size == 32

type unboxed =
  | Unboxed_int of int * int
  | Unboxed_float of float (* $f.64 cannot fit *)
  | Unboxed_null

type obj =
  | Obj_raw_data
  | Obj_tuple of value list
  | Obj_thunk of value * value list
  | Obj_other of int * value list

and value =
  | Val_unboxed of unboxed
  | Val_object of obj

let header_lltype ctx = Llvm.integer_type ctx.context 64

let lltype_of_value ctx = function
  | Val_unboxed (Unboxed_int (w, _)) ->
    Llvm.integer_type ctx.context w
  | Val_unboxed (Unboxed_float _) ->
    Llvm.float_type ctx.context
  | Val_unboxed Unboxed_null ->
    Llvm.integer_type ctx.context Sys.word_size
  | Val_object _ ->
    Llvm.pointer_type (header_lltype ctx)

let llvalue_of_value ctx value =
  let ty = lltype_of_value ctx value in
  match value with
    | Val_unboxed (Unboxed_int (_, i)) ->
      let i = ((i lsl 1) lor 0x1) in
      Llvm.const_int ty i
    | Val_unboxed (Unboxed_float f) ->
      let i = Int64.bits_of_float f in
      let i = Int64.(logor (shift_left i 1) one) in 
      Llvm.const_float ty (Int64.float_of_bits i)
    | Val_unboxed Unboxed_null ->
      let i = Int64.(logor (shift_left one 1) one) in
      Llvm.const_of_int64 ty i false
    | Val_object _ ->
      (* TODO *)
      Llvm.const_null ty
