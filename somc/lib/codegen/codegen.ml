module Print = Print

open Cmodule
open Lambda.Ir

type cmodule = Cmodule.cmodule

let codegen_expr _ = function
  | Lexpr_atom atom -> Cexpr_atom atom
  | _ -> Cexpr_atom (Latom_const Lconst_null)

let codegen_stmt m = function
  | Lstmt_definition (ident, expr) ->
    let cexpr = codegen_expr m expr in
    add_decl m (Cdecl_global (ident, cexpr))
  
  | _ -> ()

let codegen_program program =
  let m = empty_cmodule () in
  List.iter (codegen_stmt m) program;
  m