module Ir = Ir
module Lower = Lower
module Print = Print

let fresh = Env.fresh

let convert tast =
  (* let p = Lower.lower_tast tast in *)
  (* Report.report_note "LOWERED:"; *)
  (* Print.print_program p; *)
  (* *)
  (* let p = Apply.convert_statements p in *)
  (* prerr_endline "\n\n\n"; *)
  (* Report.report_note "CONVERTED:"; *)
  (* Print.print_program p; *)
  (* *)
  (* let p = Simplify.basic_simplify_pogram p in *)
  (* prerr_endline "\n\n\n"; *)
  (* Report.report_note "SIMPLIFIED:"; *)
  (* Print.print_program p; *)
  (* *)
  (* let p = Closure.convert_program p in *)
  (* prerr_endline "\n\n\n"; *)
  (* Report.report_note "CLOSURED:"; *)
  (* Print.print_program p; *)
  (* *)
  (* let p = Lifting.lift_program p in *)
  (* prerr_endline "\n\n\n"; *)
  (* Report.report_note "LIFTED:"; *)
  (* Print.print_program p; *)
  (* *)
  (* let p = Simplify.Uncurry.uncurry_program p in *)
  (* prerr_endline "\n\n\n"; *)
  (* Report.report_note "UNCURRIED:"; *)
  (* Print.print_program p; *)
  (* *)
  (* let p = Apply.convert_program p in *)
  (* prerr_endline "\n\n\n"; *)
  (* Report.report_note "CONVERTED:"; *)
  (* Print.print_program p; *)
  (* *)
  (* p *)

  let prog =
    Lower.lower_tast tast
    (* |> Simplify.basic_simplify_pogram *)
  in
  if !Configs.Cli.args.dump_raw_ir then begin
    Report.report_note "dumping raw IR:";
    Print.print_program prog;
    Report.last_was_compact := false;
    Report.has_reported := true
  end;
  
  prog
  |> Apply.convert_statements
  |> Closure.convert_program
  (* |> Simplify.basic_simplify_pogram *)
  |> Lifting.lift_program
  (* |> Simplify.basic_simplify_pogram *)
  |> Simplify.Uncurry.uncurry_program
  |> Simplify.basic_simplify_pogram
  |> Apply.convert_program
  (* |> Simplify.basic_simplify_pogram *)
