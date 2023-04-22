open Context
open Lambda.Ir

let header_lltype ctx = Llvm.integer_type ctx.context 64
let value_lltype ctx = Llvm.integer_type ctx.context Sys.word_size

let function_lltype ctx arity =
  Array.make arity (value_lltype ctx)
  |> Llvm.function_type (value_lltype ctx)

let lltype_of_const ctx = function
  | Const_int _ -> Llvm.i32_type ctx.context
  | Const_float _ -> Llvm.float_type ctx.context
  | Const_string _ -> value_lltype ctx
  | Const_null -> value_lltype ctx

let llvalue_of_const ctx const =
  let lltype = lltype_of_const ctx const in
  match const with
    | Const_int i ->
      let i = (i lsl 1) lor 0x1 in
      Llvm.const_int lltype i
    | Const_float f ->
      (* let i = Int64.bits_of_float f in
      let i = Int64.(logor (shift_left i 1) one) in 
      Llvm.const_float lltype (Int64.float_of_bits i) *)
      let i = (Obj.magic f lsl 1) lor 0x1 in
      Llvm.const_float lltype (Obj.magic i)
    | Const_string _s ->
      failwith "TODO: llvalue_of_const Const_string"
    | Const_null ->
      let i = 0 lor 0x1 in
      Llvm.const_int lltype i
