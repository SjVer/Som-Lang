module Ir = Ir
module Lower = Lower
module Print_ir = Print_ir

let convert tast =
  Lower.lower_tast tast
  |> Optimize.optimize_program
  |> Uncurry.uncurry_program
  |> Closure.convert_program
  |> Lifting.lift_program
  |> Optimize.optimize_program