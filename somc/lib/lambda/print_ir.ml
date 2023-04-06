open Ir
open Format

let fpf = fprintf
let dpf = dprintf

let print_var' ppf = function
  | Var_local v -> fpf ppf "%s" v
  | Var_global v -> fpf ppf "!%s" v

let print_atom' ppf = function
  | Atom_const (Const_int i) -> pp_print_int ppf i
  | Atom_const (Const_float f) -> pp_print_float ppf f
  | Atom_const (Const_string s) -> fpf ppf "\"%s\"" s
  | Atom_const Const_nil -> pp_print_string ppf "()"
  | Atom_var v -> print_var' ppf v

let rec fpf_step ppf r b printer =
  fpf ppf "let %s = @[<2>" r;
  printer ppf;
  fpf ppf "@] in@ @[%a@]" print_expr' b

and print_expr' ppf = function
  | Expr_lambda (r, (params, expr), b) ->
    fpf_step ppf r b begin
      dpf "\\%a ->@ @[<2>%a@]"
        (pp_print_list pp_print_string) params
        print_expr' expr
    end
  | Expr_apply (r, (func, args), b) ->
    fpf_step ppf r b begin
      dpf "apply %a %a"
        print_var' func
        (pp_print_list print_atom') args
    end
  | Expr_eval (r, var, b) ->
    fpf_step ppf r b begin
      dpf "eval %a" print_var' var
    end
  | Expr_if (r, (cond, thenexpr, elseexpr), b) ->
    fpf_step ppf r b begin
      dpf "if %a then@ @[<2>%a@]@ else @[<2>%a@]"
        print_atom' cond
        print_expr' thenexpr
        print_expr' elseexpr
    end
  | Expr_sequence (r, (e1, e2), b) ->
    fpf_step ppf r b begin
      dpf "@[%a@] ;@ @[%a@]"
        print_expr' e1
        print_expr' e2
    end
  | Expr_tuple (r, els, b) ->
    fpf_step ppf r b begin
      dpf "(%a)" (pp_print_list
       ~pp_sep:(fun ppf () -> pp_print_string ppf "; ")
       print_atom') els
    end
  | Expr_get (r, (tup, i), b) ->
    fpf_step ppf r b begin
      dpf "%a[%d]" print_var' tup i
    end
  | Expr_atom a -> print_atom' ppf a

let print_stmt' ppf = function
  | Stmt_definition (name, expr) ->
    fpf ppf "define %s =@[@;%a@]"
      name print_expr' expr
  | Stmt_external (name, native) ->
    fpf ppf "external %s = \"%s\"" name native

let print_program' ppf stmts =
  let f i stmt =
    if i <> 0 then pp_print_newline ppf ();
    print_stmt' ppf stmt;
    pp_print_newline ppf ()
  in
  List.iteri f stmts

(* expose functions *)

let print_stmt = print_stmt' std_formatter
let print_program = print_program' std_formatter