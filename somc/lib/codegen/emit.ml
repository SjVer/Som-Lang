open Cmodule
open Lambda.Ir
open Format

let mangle s =
  let is_alphanum = function 
    | '0' .. '9' | '_'
    | 'a' .. 'z' | 'A' .. 'Z' -> true 
    | _ -> false
  in
  let f str ch =
    if is_alphanum ch then str ^ String.make 1 ch
    else str ^ Format.sprintf "$%x" (Char.code ch)
  in
  String.fold_left f "" s

let fpf = fprintf
let fps = pp_print_string

let comma_sep f _ = pp_print_string f ", "

let emit_atom f = function
  | Latom_const (Lconst_int i) -> fpf f "Unboxed_val(%d)" i
  | Latom_const (Lconst_float _) -> failwith "emit atom float"
  | Latom_const (Lconst_string _) -> failwith "emit atom string"
  | Latom_const Lconst_null -> fps f "Null_val"
  | Latom_var (Lvar_local v) -> fps f (mangle v)
  | Latom_var (Lvar_global v) -> fps f (mangle v)
  | Latom_var (Lvar_tag _) -> failwith "emit atom tag"
  | Latom_prim _ -> failwith "emit atom prim"

let emit_prim f prim args =
  let binop op = match args with
    | [lhs; rhs] ->
      fpf f "@[<2>Unboxed_val(Val_value(%a) %s Val_value(%a))@]"
        emit_atom lhs op emit_atom rhs
    | _ -> failwith "emit invalid binop prim"
  in

  let open Symbols.Primitive in
  match prim with
    | Prim_add_int
    | Prim_add_char
    | Prim_add_float  -> binop "+"
    | Prim_add_string -> failwith "emit prim add_string"
    | Prim_sub_int
    | Prim_sub_char
    | Prim_sub_float  -> binop "-"
    | Prim_mul_int
    | Prim_mul_char
    | Prim_mul_float  -> binop "*"
    | Prim_div_int
    | Prim_div_char
    | Prim_div_float  -> binop "/"
    | Prim_rem_int    -> binop "%"
    | Prim_rem_float  -> failwith "emit prim rem_float"
    | Prim_abs_int
    | Prim_abs_float
    | Prim_neg_int
    | Prim_neg_float
    | Prim_and
    | Prim_or
    | Prim_not
    | Prim_eq
    | Prim_eq_value
    | Prim_gt_int
    | Prim_gt_float
    | Prim_lt_int
    | Prim_lt_float
    | Prim_neq
    | Prim_neq_value
    | Prim_gteq_int
    | Prim_gteq_float
    | Prim_lteq_int
    | Prim_lteq_float
    | Prim_tageq -> failwith "emit invalid prim"

let emit_cexpr f = function
  | Cexpr_atom atom -> emit_atom f atom
  | Cexpr_prim (prim, args) -> emit_prim f prim args
  | Cexpr_call (ident, args) ->
    fpf f "@[<2>%s (%a)@]"
      (mangle ident) 
      (pp_print_list ~pp_sep:comma_sep emit_atom) args

let emit_cstmt f = function
  | Cstmt_expr expr -> emit_cexpr f expr
  | Cstmt_assign (ident, expr) -> 
    fpf f "@[<2>value %s@ = %a@];"
      (mangle ident) emit_cexpr expr
  | Cstmt_return expr ->
    fpf f "@[<2>return@ %a@];" 
      emit_cexpr expr

let emit_cblock f block =
    List.iter (emit_cstmt f) block

let emit_cdecl f decl =
  pp_print_newline f ();
  pp_print_newline f ();
  match decl with
  | Cdecl_global (name, _) ->
    fpf f "// global `%s`@." name;
    (* fpf f "@[<2>value@ %s@ =@ %a@];"
      (mangle name) emit_cexpr expr *)
    fpf f "static value %s;" (mangle name);
  | Cdecl_function (name, params, block) ->
    fpf f "// function `%s`@." name;
    let params' = List.map (fun s -> "value " ^ mangle s) params in
    fpf f "value %s (%a) {@.@[<2>  %a@;@.}"
      (mangle name)
      (pp_print_list ~pp_sep:comma_sep pp_print_string) params'
      emit_cblock block
  | Cdecl_external (name, arity) ->
    let args = 
      Array.make arity "value"
      |> Array.to_list
      |> String.concat ", "
    in
    fpf f "// external `%s`@." name;
    fpf f "extern value %s (%s);" (mangle name) args

let emit_ctors m f =
  pp_print_newline f ();
  pp_print_newline f ();
  fps f "value som_entrypoint() { return }\n";
  fps f "void __attribute__((constructor)) _som_GLOBALS_CTOR () {\n";
  let emit_ctor = function
  | Cdecl_global (name, cexpr) ->
      fpf f "  @[<2>%s = %a;@]@." (mangle name) emit_cexpr cexpr;
    | _ -> ()
  in
  List.iter emit_ctor m.decls;
  fpf f "}@."
    
let emit_entrypoint m f =
  ignore m; ignore f

let emit_cmodule m =
  ignore (flush_str_formatter ());
  let f = str_formatter in

  fps f Configs.c_header;

  List.iter (emit_cdecl f) m.decls;
  emit_ctors m f;
  emit_entrypoint m f;

  flush_str_formatter ()