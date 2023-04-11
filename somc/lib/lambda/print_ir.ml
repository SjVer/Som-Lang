open Ir
open Format

let fpf = fprintf
let pp_list pp = pp_print_list ~pp_sep:pp_print_space pp
let kw = ANSITerminal.(sprintf [magenta]) "%s"
let var str =
  (* let var = ANSITerminal.(sprintf [green]) "%s" *)
  str

let print_var' ppf = function
  | Var_local v -> fpf ppf "%s" (var v)
  | Var_global v -> fpf ppf "%s" (var (v ^ "!"))
  | Var_tag t -> fpf ppf "<tag %d>" t

let print_atom' ppf = function
  | Atom_const (Const_int i) -> pp_print_int ppf i
  | Atom_const (Const_float f) -> pp_print_float ppf f
  | Atom_const (Const_string s) -> fpf ppf "\"%s\"" s
  | Atom_const Const_nil -> pp_print_string ppf "()"
  | Atom_var v -> print_var' ppf v
  | Atom_magic m ->
    let m' = Typing.Magicals.to_string m in
    fpf ppf "#%s" m'

let rec print_expr' ppf = function
  | Expr_let (v, value, expr) ->
    fpf ppf "@[<2>(%s@ %s@ %a@ %s@ %a)@]"
      (kw "let") (var v)
      print_expr' value
      (kw "in") print_expr' expr
  | Expr_lambda (params, expr) ->
    fpf ppf "@[<2>(%s@ %a@ %a)@]"
      (kw "lam")
      (pp_list pp_print_string) params
      print_expr' expr
  | Expr_call (func, args) ->
    fpf ppf "@[<2>(%s@ %a@ %a)@]"
      (kw "call")
      print_atom' func
      (pp_list print_atom') args
  | Expr_apply (func, args) ->
    fpf ppf "@[<2>(%s@ %a@ %a)@]"
      (kw "apply")
      print_expr' func
      (pp_list print_atom') args
  | Expr_if (cond, thenexpr, elseexpr) ->
    fpf ppf "@[<2>(%s@ %a@ %s@ %a@ %s@ %a)@]"
      (kw "if") print_atom' cond
      (kw "then") print_expr' thenexpr
      (kw "else") print_expr' elseexpr
  | Expr_sequence (e1, e2) ->
    fpf ppf "@[<2>(%s@ @[%a@ %a@])@]"
      (kw "seq")
      print_expr' e1
      print_expr' e2
  | Expr_tuple els ->
    let fpf_el ppf a =
      fpf ppf "@ %a" print_atom' a
    in
    fpf ppf "@[<2>(%s%a)@]"
      (kw "tup")
      (pp_print_list fpf_el) els
  | Expr_object (tag, els) ->
    let fpf_el ppf a =
      fpf ppf "@ %a" print_atom' a
    in
    fpf ppf "@[<2>(%s #%d%a)@]"
      (kw "object") tag
      (pp_print_list fpf_el) els
  | Expr_lazy expr ->
    fpf ppf "@[<2>(%s@ %a)@]"
      (kw "lazy")
      print_expr' expr
  | Expr_get (tup, i) ->
    fpf ppf "@[<2>(%s@ %a@ %d)@]"
      (kw "get")
      print_var' tup i
  | Expr_eval var ->
    fpf ppf "@[<2>(%s@ %a)@]"
      (kw "eval")
      print_var' var
  | Expr_atom a -> print_atom' ppf a

let print_stmt' ppf = function
  | Stmt_definition (name, expr) ->
    fpf ppf "@[<2>(%s@ %s@ %a@])"
      (kw "define")
      (var (name ^ "!"))
      print_expr' expr
  (* | Stmt_function (name, params, expr) ->
    fpf ppf "@[<2>(%s@ %s%a@ %a@])"
      (kw "function")
      (var (name ^ "!"))
      (pp_print_list (fun f -> fpf f "@ %s")) params
      print_expr' expr *)
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