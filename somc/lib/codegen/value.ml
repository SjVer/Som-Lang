open Context
open Lambda.Ir

module Tag = struct
  let max = 250
  let float = 251
  let raw_data = 252
  let record = 253
  let tuple = 254
  let closure = 255
end
module Status = struct
  let const = 0x03
end

let word_size ctx =
  Llvm_target.TargetMachine.data_layout ctx.machine
  |> Llvm_target.DataLayout.pointer_size
  |> ( * ) 8

let header_lltype ctx = Llvm.integer_type ctx.context 64
let value_lltype ctx = Llvm.integer_type ctx.context (word_size ctx)

let function_lltype ctx arity =
  Array.make arity (value_lltype ctx)
  |> Llvm.function_type (value_lltype ctx)

let get_ext_func ctx name ?(vararg=false) args =
  match Llvm.lookup_function name ctx.llmodule with
    | None ->
      let ty =
        if vararg then 
          Llvm.var_arg_function_type (value_lltype ctx) args
        else
          Llvm.function_type (value_lltype ctx) args
      in
      Llvm.declare_function name ty ctx.llmodule
    | Some func -> func

let make_raw_header tag ?(status=0) payload =
  let open Int64 in
  zero (* mind the endianness!! *)
  |> logand 0xffffffffffffff00L |> logor (of_int tag)
  |> logand 0xffffffffffff00ffL |> logor (shift_left (of_int status) 8)
  |> logand 0x00000000ffffffffL |> logor (shift_left (of_int payload) 32)

let make_header ctx tag ?(status=0) payload =
  let header = make_raw_header tag ~status payload in
  Llvm.const_of_int64 (header_lltype ctx) header false

let bytes_of_int64 ctx i =
  let bytes = Bytes.make 8 '\x00' in
  if is_big_endian ctx then Bytes.set_int64_be bytes 0 i
  else Bytes.set_int64_le bytes 0 i;
  bytes

let make_const_obj ctx tag payload data =
    let h = make_raw_header tag ~status:Status.const payload in
    let hbytes = bytes_of_int64 ctx h in
    let bytes = Bytes.cat hbytes data in
    
    (* encode obj's bytes as array *)
    let t = Llvm.i8_type ctx.context in
    let arr =
      Array.of_seq (Bytes.to_seq bytes)
      |> Array.map int_of_char
      |> Array.map (Llvm.const_int t)
      |> Llvm.const_array (Llvm.i8_type ctx.context)
    in
    let obj = Llvm.define_global "obj" arr ctx.llmodule in

    (* value is pointer to obj *)
    Llvm.const_bitcast obj (value_lltype ctx)  

let llvalue_of_const ctx const =
  let lltype = value_lltype ctx in
  match const with
    | Const_int i ->
      let open Int64 in
      let i' = of_int i in
      let i' = logor (shift_left i' 1) one in
      Llvm.const_of_int64 lltype i' false
    | Const_float f ->
      let i = Int64.bits_of_float f in
      let data = bytes_of_int64 ctx i in
      make_const_obj ctx Tag.float 0 data
    | Const_string s ->
      let data = Bytes.of_string s in
      make_const_obj ctx Tag.raw_data (Bytes.length data) data
    | Const_null ->
      Llvm.const_null lltype
    
let make_fields_obj ctx tag fields =
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
