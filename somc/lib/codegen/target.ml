open Llvm_target

let triple = ref ""
let name = ref ""
let datalayout = ref ""
let is_big_endian = ref false
let word_size = ref 0

let setup () =
  let triple' = match !Configs.Cli.args.target with
    | Some triple -> triple
    | None -> Target.default_triple ()
  in
  let target = Target.by_triple !triple in 
  let machine = TargetMachine.create ~triple:!triple target in
  let datalayout' = TargetMachine.data_layout machine in

  triple := triple';
  name := Target.name target;
  datalayout := DataLayout.as_string datalayout';
  is_big_endian := DataLayout.byte_order datalayout' = Endian.Big;
  word_size := DataLayout.pointer_size datalayout';
  
  if !Configs.Cli.args.verbose then begin
    Printf.sprintf "target: %s (%s) (%d bit)"
      !name !triple (!word_size * 8)
    |> Report.report_note
  end