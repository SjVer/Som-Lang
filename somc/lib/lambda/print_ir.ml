open Ir
open Format

let fpf = fprintf
let dpf = dprintf
let kw = ANSITerminal.(sprintf [magenta]) "%s"
let var str =
  (* let var = ANSITerminal.(sprintf [green]) "%s" *)
  str

let print_var' ppf = function
  | Var_local v -> fpf ppf "%s" (var v)
  | Var_global v -> fpf ppf "%s" (var (v ^ "!"))

let print_atom' ppf = function
  | Atom_const (Const_int i) -> pp_print_int ppf i
  | Atom_const (Const_float f) -> pp_print_float ppf f
  | Atom_const (Const_string s) -> fpf ppf "\"%s\"" s
  | Atom_const Const_nil -> pp_print_string ppf "()"
  | Atom_var v -> print_var' ppf v

let rec fpf_step ppf r b printer =
  fpf ppf "@[<2>(%s@ %s@ " (kw "let") (var r);
  printer ppf;
  fpf ppf "@ %s@ %a)@]" (kw "in") print_expr' b

and print_expr' ppf = function
  | Expr_lambda (r, (params, expr), b) ->
    fpf_step ppf r b begin
      dpf "@[<2>(%s@ %a@ %a)@]"
        (kw "lam")
        (pp_print_list pp_print_string) params
        print_expr' expr
    end
  | Expr_apply (r, (func, args), b) ->
    fpf_step ppf r b begin
      dpf "@[<2>(%s@ %a@ %a)@]"
        (kw "apply")
        print_atom' func
        (pp_print_list print_atom') args
    end
  | Expr_eval (r, var, b) ->
    fpf_step ppf r b begin
      dpf "@[<2>(%s@ %a)@]"
        (kw "eval")
        print_var' var
    end
  | Expr_if (r, (cond, thenexpr, elseexpr), b) ->
    fpf_step ppf r b begin
      dpf "@[<2>(%s@ %a@ %s@ %a@ %s@ %a)@]"
        (kw "if") print_atom' cond
        (kw "then") print_expr' thenexpr
        (kw "else") print_expr' elseexpr
    end
  | Expr_sequence (r, (e1, e2), b) ->
    fpf_step ppf r b begin
      dpf "@[<2>(%s@ @[%a@ %a@])@]"
        (kw "seq")
        print_expr' e1
        print_expr' e2
    end
  | Expr_tuple (r, els, b) ->
    let fpf_el ppf a =
      fpf ppf "@ %a" print_atom' a
    in
    fpf_step ppf r b begin
      dpf "@[<2>(%s%a)@]"
        (kw "tup")
        (pp_print_list fpf_el) els
    end
  | Expr_get (r, (tup, i), b) ->
    fpf_step ppf r b begin
      dpf "@[<2>(%s@ %a@ %d)@]"
        (kw "get")
        print_var' tup i
    end
  | Expr_atom a -> print_atom' ppf a

let print_stmt' ppf = function
  | Stmt_definition (name, expr) ->
    fpf ppf "@[<2>(%s@ %s@ %a@])"
      (kw "define") (var (name ^ "!")) print_expr' expr
  | Stmt_external (name, native) ->
    fpf ppf "(%s %s %s)"
      (kw "extern") (var (name ^ "!")) native

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