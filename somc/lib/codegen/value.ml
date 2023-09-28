open Cmodule

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

let add_builtin m name args =
  match lookup_builtin m name with
    | Some args' ->
      if args <> args' then
        failwith ("builtin " ^ name ^ " declared twice")
      else ()
    | None -> add_builtin m name args

let make_header tag ?(status=0) payload =
  let open Int64 in
  zero (* mind the endianness!! *)
  |> logand 0xffffffffffffff00L |> logor (of_int tag)
  |> logand 0xffffffffffff00ffL |> logor (shift_left (of_int status) 8)
  |> logand 0x00000000ffffffffL |> logor (shift_left (of_int payload) 32)

let bytes_of_int64 i =
  let bytes = Bytes.make 8 '\x00' in
  if !Target.is_big_endian then Bytes.set_int64_be bytes 0 i
  else Bytes.set_int64_le bytes 0 i;
  bytes
