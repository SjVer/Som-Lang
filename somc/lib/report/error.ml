let f = Util.f

(* comments after variants for gen_codes_ml.py *)

type lexing_error =
  | Unexpected_character of string
  | Illegal_escape of char (*+sequence*)
  | Unterminated_string
  | Invalid_literal of string
  | Invalid_builtin_type of string

let get_lexing_error_msg = function
  | Unexpected_character w  -> f "unexpected character '%s'" w
  | Illegal_escape w        -> f "illegal escape sequence '\\%c'" w
  | Unterminated_string     -> f "unterminated string"
  | Invalid_literal w       -> f "invalid literal '%s'" w
  | Invalid_builtin_type w  -> f "invalid builtin type '%s'" w

type syntax_error =
  | Expected of string (*+token*)
  | Unexpected (*+token*)
  | Expected_toplevel
  | Expected_expression
  | Expected_type
  | Unclosed of string (*+token*)
  | Use_of_unbound of string (*+symbol*)
  | Duplicate_parameter of string
  | Other of string (*+syntax error*)

let get_syntax_error_msg = function
  | Expected w            -> f "expected '%s'" w
  | Unexpected            -> f "unexpected token"
  | Expected_toplevel     -> f "expected top-level"
  | Expected_expression   -> f "expected expression"
  | Expected_type         -> f "expected type"
  | Unclosed w            -> f "unclosed '%s'" w
  | Use_of_unbound w      -> f "use of unbound value `%s`" w
  | Duplicate_parameter w -> f "duplicate parameter `%s`" w
  | Other w               -> w

type other_error =
  | Could_not_open of string (*+file*)
  | Could_not_compile of string (*+file*)
  | Failed_to_resolve of string (*+module*)
  | Failed_to_import of string (*+module*)
  | Cannot_explain of int (*+error code*)

let get_other_error_msg = function
  | Could_not_open w    -> f "could not open file '%s'" w
  | Could_not_compile w -> f "could not compile '%s' due to previous error" w
  | Failed_to_resolve w -> f "failed to resolve module '%s'" w
  | Failed_to_import w  -> f "failed to import module `%s`" w
  | Cannot_explain w    -> f "cannot explain invalid error code E%03d" w

type error =
  | Lexing_error of lexing_error
  | Syntax_error of syntax_error
  | Other_error of other_error

let get_error_header_and_msg = function
  | Lexing_error e -> "lexing error", get_lexing_error_msg e
  | Syntax_error e -> "syntax error", get_syntax_error_msg e
  | Other_error e -> "error", get_other_error_msg e

type note = string

exception Error of error * Span.span option * note list

let raise_error err span notes = raise (Error (err, span, notes))