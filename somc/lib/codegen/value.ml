open Context
open Lambda.Ir

let header_lltype ctx = Llvm.integer_type ctx.context 64
let value_lltype ctx =
  Llvm_target.TargetMachine.data_layout ctx.machine
  |> Llvm_target.DataLayout.pointer_size
  |> ( * ) 8
  |> Llvm.integer_type ctx.context

let function_lltype ctx arity =
  Array.make arity (value_lltype ctx)
  |> Llvm.function_type (value_lltype ctx)

let get_ext_func ctx name args =
  match Llvm.lookup_function name ctx.llmodule with
    | None ->
      let ty = Llvm.(function_type (value_lltype ctx) args) in
      Llvm.declare_function name ty ctx.llmodule
    | Some func -> func

let llvalue_of_const ctx const =
  let lltype = value_lltype ctx in
  let open Int64 in
  let encode_const i =
     let i = logor (shift_left i 1) one in
     Llvm.const_of_int64 lltype i false
  in
  match const with
    | Const_int i -> encode_const (of_int i)
    | Const_float f -> encode_const (bits_of_float f)
    | Const_string s ->
      let ptr = Llvm.build_global_stringptr s "str" ctx.builder in
      let som_make_str = get_ext_func ctx "som_str_make" [|Llvm.type_of ptr|] in
      Llvm.build_call som_make_str [|ptr|] "strval" ctx.builder
    | Const_null -> encode_const zero

let make_header ctx tag payload =
  let open Int64 in
  let header = 
    zero (* mind the endianness!! *)
    |> logand 0xffffffffffffff00L |> logor (of_int tag)
    |> logand 0x00000000ffffffffL |> logor (shift_left (of_int payload) 32)
  in
  Llvm.const_of_int64 (header_lltype ctx) header false
    
let make_object ctx tag fields =
  (* malloc the object *)
  let size =
    let hbits = Llvm.integer_bitwidth (header_lltype ctx) in
    let vbits = Llvm.integer_bitwidth (value_lltype ctx) in
    let size = (hbits + List.length fields * vbits) / 8 in
    Llvm.const_int (Llvm.i32_type ctx.context) size
  in
  let malloc_fn = get_ext_func ctx "som_malloc" [|Llvm.type_of size|] in
  let value = Llvm.build_call malloc_fn [|size|] "value" ctx.builder in
  
  (* write the header *)
  let header = make_header ctx tag (List.length fields) in
  ignore (Llvm.build_store header value ctx.builder);

  (* write the fields *)
  let object_addr =
    Llvm.pointer_type (Llvm.i8_type ctx.context)
    |> Llvm.const_inttoptr value
  in
  let store_field i v =
    (* calculate the ptr offsets manually *)
    let offset =
      Llvm.const_int
      (Llvm.i32_type ctx.context)
      (8 + i * 8)
    in
    let addr = Llvm.build_add object_addr offset "field_addr" ctx.builder in
    let ptr = Llvm.build_inttoptr addr
      (Llvm.pointer_type (value_lltype ctx))
      "field_ptr" ctx.builder
    in 
    ignore (Llvm.build_store v ptr ctx.builder)
  in
  List.iteri store_field fields;

  value
