module Ir = Ir
module Lower = Lower
module Print = Print

let convert tast =
  let prog = Lower.lower_tast tast in
  (* Print.print_program prog;

  let prog = Simplify.basic_simplify_pogram prog in
  print_endline "\n==== Simplified basics ====";
  Print.print_program prog;

  let prog = Uncurry.uncurry_program prog in
  print_endline "\n==== Uncurried ====";
  Print.print_program prog;

  let prog = Closure.convert_program prog in
  print_endline "\n==== Closure converted ====";
  Print.print_program prog;

  let prog = Lifting.lift_program prog in
  print_endline "\n==== Lambda lifted ====";
  Print.print_program prog;

  let prog = Simplify.LetAlias.simplify_program prog in
  print_endline "\n==== Simplified let aliases ====";
  Print.print_program prog;

  print_endline "\n==== Final program ====";
  prog *)

  prog
  |> Simplify.basic_simplify_pogram
  |> Simplify.ApplyToCall.simplify_program
  |> Uncurry.uncurry_program