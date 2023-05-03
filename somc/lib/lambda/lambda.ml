module Ir = Ir
module Lower = Lower
module Print = Print

let convert tast =
  Lower.lower_tast tast
  |> Simplify.basic_simplify_pogram
  |> Closure.convert_program
  |> Simplify.basic_simplify_pogram
  |> Lifting.lift_program
  |> Simplify.basic_simplify_pogram
  |> Simplify.Uncurry.uncurry_program
  |> Simplify.basic_simplify_pogram
