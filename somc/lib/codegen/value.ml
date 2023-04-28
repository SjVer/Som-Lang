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
  let open Int64 in
  let encode_const i =
     let i = logor (shift_left i 1) Int64.one in
     Llvm.const_of_int64 lltype i false
  in
  match const with
    | Const_int i -> encode_const (of_int i)
    | Const_float f -> encode_const (bits_of_float f)
    | Const_string s ->
      let ptr = Llvm.build_global_stringptr s "str" ctx.builder in
      let som_make_str =
        let name = "_som_make_str" in
        match Llvm.lookup_function name ctx.llmodule with
          | None ->
            let ty = Llvm.(function_type lltype [|type_of ptr|]) in
            Llvm.declare_function name ty ctx.llmodule
          | Some func -> func
      in
      Llvm.build_call som_make_str [|ptr|] "strval" ctx.builder
    | Const_null -> encode_const zero
