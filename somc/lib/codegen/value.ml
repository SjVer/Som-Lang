open Context
open Lambda.Ir

let header_lltype ctx = Llvm.integer_type ctx.context 64
let value_lltype ctx = Llvm.integer_type ctx.context Sys.word_size

let function_lltype ctx arity =
  Array.make arity (value_lltype ctx)
  |> Llvm.function_type (value_lltype ctx)

let get_ext_func ctx name args =
  match Llvm.lookup_function name ctx.llmodule with
    | None ->
      let ty = Llvm.(function_type (value_lltype ctx) args) in
      Llvm.declare_function name ty ctx.llmodule
    | Some func -> func

let get_magic_func ctx magic =
  let open Symbols.Magic in
  let name = "_som_magic_" ^ to_string magic in
  let args = Array.make (arity magic) (value_lltype ctx) in
  get_ext_func ctx name args

let llvalue_of_const ctx const =
  let lltype = value_lltype ctx in
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
      let som_make_str = get_ext_func ctx "_som_make_str" [|Llvm.type_of ptr|] in
      Llvm.build_call som_make_str [|ptr|] "strval" ctx.builder
    | Const_null -> encode_const zero
