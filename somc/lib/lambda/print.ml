open Ir
open Format

let fpf = fprintf
let pp_list pp = pp_print_list ~pp_sep:pp_print_space pp
let kw = ANSITerminal.(sprintf [magenta]) "%s"
let ex = ANSITerminal.(sprintf [blue]) "%s"
let var str = str

let print_var' ppf = function
  | Lvar_local v -> fpf ppf "%s" (var v)
  | Lvar_global v -> fpf ppf "%s" (var (v ^ "!"))
  | Lvar_tag t -> fpf ppf "<tag %d>" t

let print_atom' ppf = function
  | Latom_const (Lconst_int i) -> pp_print_int ppf i
  | Latom_const (Lconst_float f) -> pp_print_float ppf f
  | Latom_const (Lconst_string s) -> fpf ppf "\"%s\"" s
  | Latom_const Lconst_null -> fpf ppf "%s" (kw "null")
  | Latom_var v -> print_var' ppf v
  | Latom_prim p -> fpf ppf "#%s" (Symbols.Primitive.to_string p)

let rec print_case' ppf (tag, expr) =
  fpf ppf "@[<2>(%s@ %d:@ %a@)@]"
    (kw "case") tag
    print_expr' expr

and print_expr' ppf = function
  | Lexpr_let (v, value, expr) ->
    fpf ppf "@[<2>(%s@ %s@ %a@ %s@ %a)@]"
      (kw "let") (var v)
      print_expr' value
      (kw "in") print_expr' expr
  | Lexpr_lambda (params, expr) ->
    fpf ppf "@[<2>(%s@ %a@ %a)@]"
      (kw "lam")
      (pp_list pp_print_string) params
      print_expr' expr
  | Lexpr_match (scrut, cases) ->
    fpf ppf "@[<2>(%s@ %a@ %a)@]"
      (kw "match")
      print_atom' scrut
      (pp_list print_case') cases
  | Lexpr_call (func, args) ->
    fpf ppf "@[<2>(%s@ %a@ %a)@]"
      (kw "call")
      print_atom' func
      (pp_list print_expr') args
  | Lexpr_apply (func, args) ->
    fpf ppf "@[<2>(%s@ %a@ %a)@]"
      (kw "apply")
      print_expr' func
      (pp_list print_expr') args
  | Lexpr_if (cond, thenexpr, elseexpr) ->
    fpf ppf "@[<2>(%s@ %a@ @[<2>%s@ %a@]@ @[<2>%s@ %a)@]@]"
      (kw "if") print_expr' cond
      (kw "then") print_expr' thenexpr
      (kw "else") print_expr' elseexpr
  | Lexpr_sequence (e1, e2) ->
    fpf ppf "@[<2>(%s@ @[%a@ %a@])@]"
      (kw "seq")
      print_expr' e1
      print_expr' e2
  | Lexpr_tuple els ->
    let fpf_el ppf a =
      fpf ppf "@ %a" print_atom' a
    in
    fpf ppf "@[<2>(%s%a)@]"
      (kw "tup")
      (pp_print_list fpf_el) els
  | Lexpr_object (tag, els) ->
    let fpf_el ppf a =
      fpf ppf "@ %a" print_atom' a
    in
    fpf ppf "@[<2>(%s #%d%a)@]"
      (kw "object") tag
      (pp_print_list fpf_el) els
  | Lexpr_lazy expr ->
    fpf ppf "@[<2>(%s@ %a)@]"
      (kw "lazy")
      print_expr' expr
  | Lexpr_get (tup, i) ->
    fpf ppf "@[<2>(%s@ %a@ %d)@]"
      (kw "get")
      print_var' tup i
  | Lexpr_eval var ->
    fpf ppf "@[<2>(%s@ %a)@]"
      (kw "eval")
      print_var' var
  | Lexpr_atom a -> print_atom' ppf a
  | Lexpr_fail ->
    fpf ppf "(%s)" (kw "fail")

let print_stmt' ppf = function
  | Lstmt_definition (name, expr) ->
    fpf ppf "@[<2>(%s@ %s@ %a@])"
      (kw "define")
      (var (name ^ "!"))
      print_expr' expr
  | Lstmt_function (name, params, expr) ->
    fpf ppf "@[<2>(%s@ %s%a@ %a@])"
      (kw "function")
      (var (name ^ "!"))
      (pp_print_list (fun f -> fpf f "@ %s")) params
      print_expr' expr
  | Lstmt_external (name, native, arity) ->
      fpf ppf "(%s %s %s [%d])"
      (kw "extern") (var (name ^ "!")) native arity

let print_program' ppf stmts =
  let f i stmt =
    if i <> 0 then pp_print_newline ppf ();
    print_stmt' ppf stmt;
    pp_print_newline ppf ()
  in
  List.iteri f stmts

(* expose functions *)

let print_stmt = print_stmt' err_formatter 
let print_program = print_program' err_formatter
