open Context
module C = Configs.Cli

let write_object_file ctx =
  let output_file =
    let base =
      Filename.basename !C.args.file
      |> Filename.chop_extension
    in
    if !C.args.output_obj then
      match !C.args.output with
        | None -> base ^ Configs.obj_ext
        | Some file -> file
    else 
      Filename.temp_file ("somc_" ^ base ^ "_") Configs.obj_ext
  in
  
  if !C.args.output_obj || !C.args.verbose then
    Report.report_note ("writing object file: " ^ output_file);
  if not !C.args.dry_run then
    Llvm_target.TargetMachine.emit_to_file
      ctx.llmodule 
      Llvm_target.CodeGenFileType.ObjectFile
      output_file ctx.machine;
  
  output_file

let write_asm_file ctx =
  let output_file = match !C.args.output with
    | None ->
      let base =
        Filename.basename !C.args.file
        |> Filename.chop_extension
      in 
      base ^ Configs.asm_ext
    | Some file -> file
  in
  
  if !C.args.verbose then
    Report.report_note ("writing assembly file: " ^ output_file);
  if not !C.args.dry_run then
    Llvm_target.TargetMachine.emit_to_file
      ctx.llmodule 
      Llvm_target.CodeGenFileType.AssemblyFile
      output_file ctx.machine

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
  
  if !C.args.verbose then begin
    Report.report_note ("linker: " ^ Configs.ld_path);
    (* prerr_endline command; Report.last_was_compact := false; *)
  end;

  Report.report_note ("linking executable: " ^ out_file);
  if not !C.args.dry_run then begin
    if Sys.command command <> 0 then
      let e = Report.Error.(Other_error (Other "linker failed")) in
      Report.make_error e None |> Report.report
  end;

  Sys.remove obj_file

let emit ctx =
  if !C.args.output_asm then 
    write_asm_file ctx
  else 
    let obj_file = write_object_file ctx in
    if not !C.args.output_obj then
      link_executable obj_file
