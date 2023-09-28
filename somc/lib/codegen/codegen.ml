module Print = Print
module Write = Write

open Cmodule
open Lambda.Ir

type cmodule = Cmodule.cmodule

let current_block = ref []
let add_stmt stmt =
  current_block := !current_block @ [stmt]

let rec wrap_exprs_in_atoms = function
  | [] -> []
  | Cexpr_atom atom :: atoms ->
    atom :: wrap_exprs_in_atoms atoms
  | expr :: atoms ->
    let r = Lambda.fresh () in
    add_stmt (Cstmt_assign (r, expr));
    Latom_var (Lvar_local r) :: wrap_exprs_in_atoms atoms

let rec codegen_expr m = function
  | Lexpr_let _ -> failwith "codegen Lexpr_let"
  | Lexpr_lambda _ -> failwith "codegen Lexpr_lambda"
  | Lexpr_match _ -> failwith "codegen Lexpr_match"

  | Lexpr_call (Latom_prim p, args) ->
    let arity = Symbols.Primitive.arity p in
    if List.length args <> arity then 
      failwith "codegen primitive call invalid";
    let args' = 
      List.map (codegen_expr m) args
      |> wrap_exprs_in_atoms
    in
    Cexpr_prim (p, args')

  | Lexpr_call (Latom_var (Lvar_global f), args) ->
    let args' = 
      List.map (codegen_expr m) args
      |> wrap_exprs_in_atoms
    in
    Cexpr_call (f, args')

  | Lexpr_call _ -> failwith "codegen invalid call"

  | Lexpr_apply _ -> failwith "codegen Lexpr_apply"
  | Lexpr_if _ -> failwith "codegen Lexpr_if"
  | Lexpr_sequence _ -> failwith "codegen Lexpr_sequence"
  | Lexpr_tuple _ -> failwith "codegen Lexpr_tuple"
  | Lexpr_object _ -> failwith "codegen Lexpr_object"
  | Lexpr_lazy _ -> failwith "codegen Lexpr_lazy"
  | Lexpr_get _ -> failwith "codegen Lexpr_get"
  | Lexpr_eval _ -> failwith "codegen Lexpr_eval"

  | Lexpr_atom atom -> Cexpr_atom atom

  | _ -> Cexpr_atom (Latom_const Lconst_null)

let codegen_stmt m = function
  | Lstmt_definition (ident, expr) ->
    let cexpr = codegen_expr m expr in
    add_decl m (Cdecl_global (ident, cexpr))

  | Lstmt_function (ident, args, expr) ->
    current_block := [];
    let result = codegen_expr m expr in
    add_stmt (Cstmt_return result);
    add_decl m (Cdecl_function (ident, args, !current_block))
  
  | Lstmt_external (_, ident, arity) ->
    add_decl m (Cdecl_external (ident, arity))

let codegen_program program =
  let m = empty_cmodule () in
  List.iter (codegen_stmt m) program;
  m