open Cmodule
open Lambda.Print
open Format

let comma_sep ppf _ = pp_print_string ppf ", "

let print_cexpr' ppf = function
  | Cexpr_atom atom -> print_atom' ppf atom
  | Cexpr_prim (prim, args) ->
    fpf ppf "@[<2>(%s@ #%s@ %a)@]"
      (kw "prim")
      (Symbols.Primitive.to_string prim)
      (pp_list print_atom') args
  | Cexpr_call (callee, args) ->
    fpf ppf "@[<2>(%s@ %s@ %a)@]"
      (kw "call")
      (var callee)
      (pp_list print_atom') args

let print_cstmt' ppf = function
  | Cstmt_expr expr ->
    fpf ppf "@[<2>%a@];" print_cexpr' expr
  | Cstmt_assign (ident, expr) -> 
    fpf ppf "@[<2>%s@ = %a@];"
      (var ident) print_cexpr' expr
  | Cstmt_return expr ->
    fpf ppf "@[<2>%s@ %a@];" 
      (kw "return")
      print_cexpr' expr

let print_cblock' ppf block =
    List.iter (print_cstmt' ppf) block

let print_cdecl' ppf = function
  | Cdecl_global (name, expr) ->
    fpf ppf "@[<2>%s@ %s!@ =@ %a@];"
      (kw "global")
      (var name)
      print_cexpr' expr
  | Cdecl_function (name, params, block) ->
    fpf ppf "@[<2>@[%s %s! (%a) {@]@ %a@]@ };"
      (kw "function")
      (var name)
      (pp_print_list ~pp_sep:comma_sep pp_print_string) params
      print_cblock' block
  | Cdecl_external (name, arity) ->
      fpf ppf "%s %s! [%d];"
      (kw "extern") (var name) arity

let print_cmodule' ppf cmodule =
  let f i stmt =
    if i <> 0 then pp_print_newline ppf ();
    print_cdecl' ppf stmt;
    pp_print_newline ppf ()
  in
  List.iteri f cmodule.decls

(* expose functions *)

let print_cmodule = print_cmodule' err_formatter
