module Ir = Ir
module Lower = Lower
module Print_ir = Print_ir

let convert tast =
  let prog = Lower.lower_tast tast in
  (* Print_ir.print_program prog;

  let prog = Simplify.basic_simplify_pogram prog in
  print_endline "\n==== Simplified basics ====";
  Print_ir.print_program prog;

  let prog = Uncurry.uncurry_program prog in
  print_endline "\n==== Uncurried ====";
  Print_ir.print_program prog;

  let prog = Closure.convert_program prog in
  print_endline "\n==== Closure converted ====";
  Print_ir.print_program prog;

  let prog = Lifting.lift_program prog in
  print_endline "\n==== Lambda lifted ====";
  Print_ir.print_program prog;

  let prog = Simplify.LetAlias.simplify_program prog in
  print_endline "\n==== Simplified let aliases ====";
  Print_ir.print_program prog;

  print_endline "\n==== Final program ====";
  prog *)

  Simplify.basic_simplify_pogram prog