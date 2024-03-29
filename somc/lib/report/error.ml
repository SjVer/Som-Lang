let _f = Util.f

(* comments after variants for gen_codes_ml.py *)

(* Lexing errors *)

type lexing_error =
  | Unexpected_character of string
  | Illegal_escape of char (*+sequence*)
  | Unterminated_string
  | Invalid_literal of string

let get_lexing_error_msg = function
  | Unexpected_character w  -> _f "unexpected character '%s'" w
  | Illegal_escape w        -> _f "illegal escape sequence '\\%c'" w
  | Unterminated_string     -> _f "unterminated string"
  | Invalid_literal w       -> _f "invalid literal '%s'" w

(* Syntax errors *)

type syntax_error =
  | Expected of string (*+token*)
  | Unexpected (*+token*)
  | Unclosed of string (*+token*)
  | Other of string (*+syntax error*)
  
let get_syntax_error_msg = function
  | Expected w            -> _f "expected %s" w
  | Unexpected            -> _f "unexpected token"
  | Unclosed w            -> _f "unclosed %s" w
  | Other w               -> w
  
(* Type errors *)

type type_error =
  | Expected of string * string (*+a different type*)
  | Expected_function of string (*+type*)
  | Recursive_type
  | Use_of_unbound of string * string (*+symbol*)
  | Use_of_invalid_primitive of string
  | Has_no_symbol of string * string * string (*-module has no symbol*)
  | Cannot_private of string * string (*-cannot use private symbol*)
  | Cannot_import_from of string (*+non-module symbol*)
  | Could_not_infer (*+type*)
  | Other of string (*+type error*)
  
let get_type_error_msg = function
  | Expected (e, g)          -> _f "expected type `%s` but found type `%s`" e g
  | Expected_function g      -> _f "expected a function type but found type `%s`" g
  | Recursive_type           -> _f "recursive type"
  | Use_of_unbound (t, w)    -> _f "use of unbound %s `%s`" t w
  | Use_of_invalid_primitive w -> _f "use of invalid primitive '%s'" w
  | Has_no_symbol (m, w, s)  -> _f "module `%s` has no %s named '%s'" m w s
  | Cannot_private (a, w)    -> _f "cannot %s private symbol `%s`" a w
  | Cannot_import_from w     -> _f "cannot import from non-module symbol `%s`" w
  | Could_not_infer          -> _f "could not infer type"
  | Other w                  -> w

(* Other errors *)

type other_error =
  | Could_not_open of string (*+file*)
  | Cannot_import_dir of string (*-cannot import directory*)
  | Could_not_compile of string (*+file*)
  | Failed_to_import of string (*+symbol*)
  | Cannot_explain of int (*+error code*)
  | Nonexistent_pass of string
  | Invalid_opt_level of string (*+optimization level*)
  | Other of string (*+error*)

let get_other_error_msg = function
  | Could_not_open w    -> _f "could not open file '%s'" w
  | Cannot_import_dir w -> _f "cannot import directory '%s'" w
  | Could_not_compile w -> _f "could not compile '%s' due to previous error" w
  | Failed_to_import w  -> _f "failed to import `%s`" w
  | Cannot_explain w    -> _f "cannot explain invalid error code E%03d" w
  | Nonexistent_pass w  -> _f "cannot run nonexistend LLVM pass '%s'" w
  | Invalid_opt_level w -> _f "invalid optimization level '%s'" w
  | Other w             -> w

(* Types *)

type t =
  | Lexing_error of lexing_error
  | Syntax_error of syntax_error
  | Type_error of type_error
  | Other_error of other_error

let get_header_and_msg = function
  | Lexing_error e -> "lexing error", get_lexing_error_msg e
  | Syntax_error e -> "syntax error", get_syntax_error_msg e
  | Type_error e   -> "type error",   get_type_error_msg e
  | Other_error e  -> "error",        get_other_error_msg e