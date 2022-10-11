
(* entrypoint *)
let () =
  Typing.PrintTAst.print_toplevel (
    Pipeline.TypecheckFile.call
      (!Config.Cli.args).file
  );
  exit 0