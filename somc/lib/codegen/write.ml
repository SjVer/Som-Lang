module C = Configs.Cli

let write_c_file m =
  let output_file = match !C.args.output with
    | None ->
      let base =
        Filename.basename !C.args.file
        |> Filename.chop_extension
      in 
      base ^ Configs.c_ext
    | Some file -> file
  in
  
  if !C.args.verbose then
    Report.report_note ("writing c file: " ^ output_file);
    if not !C.args.dry_run then begin
      let c_src = Emit.emit_cmodule m in
      let chan = open_out output_file in
      output_string chan c_src;
      close_out chan
    end

let write_cmodule m =
  write_c_file m
