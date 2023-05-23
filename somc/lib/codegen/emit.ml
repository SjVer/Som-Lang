module C = Configs.Cli

let write_object_file llmodule =
  Llvm_all_backends.initialize ();
  
  let triple = match !C.args.target with
    | Some triple -> triple
    | None -> Llvm_target.Target.default_triple ()
  in
  let target = Llvm_target.Target.by_triple triple in 
  let machine = Llvm_target.TargetMachine.create ~triple target in
  
  if !C.args.verbose then
    Printf.sprintf "target: %s (%s)"
      (Llvm_target.Target.name target) triple
    |> Report.report_note;

  let output_file =
    if !C.args.output_obj then
      match !C.args.output with
        | None ->
          let base =
            Filename.basename !C.args.file
            |> Filename.chop_extension
          in
          base ^ Configs.obj_ext
        | Some file -> file
    else 
      Filename.temp_file "somc_obj_" ".o"
  in
  
  if !C.args.output_obj or !C.args.verbose then
    Report.report_note ("writing object file: " ^ output_file);
  if not !C.args.dry_run then
    Llvm_target.TargetMachine.emit_to_file
      llmodule Llvm_target.CodeGenFileType.ObjectFile
      output_file machine;
  output_file

let link_executable obj_file =
  let out_file = match !C.args.output with
    | None ->
      let base =
         Filename.basename !C.args.file
         |> Filename.chop_extension
      in
      base ^ Configs.exe_ext
    | Some file -> file
  in
  let command = Printf.sprintf "%s \"%s\" -o \"%s\" %s"
    Configs.ld_path obj_file out_file
    (String.concat " " Configs.ld_args)
  in
  
  if !C.args.verbose then
    Report.report_note ("linker: " ^ Configs.ld_path);
  Report.report_note ("linking executable: " ^ out_file);
  if not !C.args.dry_run then
    ignore (Sys.command command);
  Sys.remove obj_file

let emit llmodule =
  let obj_file = write_object_file llmodule in
  if not !C.args.output_obj then
    link_executable obj_file
