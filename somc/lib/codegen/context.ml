open Llvm_target
module SMap = Map.Make(String)

type ctx =
  {
    context: Llvm.llcontext;
    machine: Llvm_target.TargetMachine.t;
    llmodule: Llvm.llmodule;
    builder: Llvm.llbuilder;
    values: Llvm.llvalue SMap.t;
  }

let is_big_endian ctx =
  TargetMachine.data_layout ctx.machine
  |> DataLayout.byte_order |> function
    | Endian.Big -> true
    | Endian.Little -> false

let make name =
  let context = Llvm.create_context () in
  
  (* we need the word size during codegen *)
  let triple = match !Configs.Cli.args.target with
    | Some triple -> triple
    | None -> Target.default_triple ()
  in
  let target = Target.by_triple triple in 
  let machine = TargetMachine.create 
    ~triple target 
    (* without this strings break. idk why *)
    ~reloc_mode:RelocMode.DynamicNoPIC
  in
  
  if !Configs.Cli.args.verbose then begin
    let bits = 
      TargetMachine.data_layout machine
      |> DataLayout.pointer_size |> ( * ) 8
    in
    Printf.sprintf "target: %s (%s) (%d bit)"
      (Target.name target) triple bits
    |> Report.report_note
  end;

  {
    context;
    machine;
    llmodule = Llvm.create_module context name;
    builder = Llvm.builder context;
    values = SMap.empty;
  }
