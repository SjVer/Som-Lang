(* File generated by gen_codes_ml.py on Thu Apr  6 17:54:53 2023. *)

open Error

let code_from_lexing_error : lexing_error -> int = function
  | Unexpected_character _ -> 101
  | Illegal_escape _ -> 102
  | Unterminated_string -> 103
  | Invalid_literal _ -> 104
  | Invalid_builtin_type _ -> 105

let code_from_syntax_error : syntax_error -> int = function
  | Expected _ -> 201
  | Unexpected -> 202
  | Unclosed _ -> 203
  | Other _ -> 204

let code_from_type_error : type_error -> int = function
  | Expected _ -> 301
  | Expected_function _ -> 302
  | Recursive_type -> 303
  | Use_of_unbound _ -> 304
  | Use_of_invalid_magical _ -> 305
  | Has_no_symbol _ -> 306
  | Cannot_private _ -> 307
  | Cannot_import_from _ -> 308
  | Could_not_infer -> 309
  | Other _ -> 310

let code_from_other_error : other_error -> int = function
  | Could_not_open _ -> 401
  | Cannot_import_dir _ -> 402
  | Could_not_compile _ -> 403
  | Failed_to_import _ -> 404
  | Cannot_explain _ -> 405
  | Nonexistent_pass _ -> 406
  | Invalid_opt_level _ -> 407
  | Other _ -> 408

let code_from_error = function
  | Lexing_error e -> code_from_lexing_error e
  | Syntax_error e -> code_from_syntax_error e
  | Type_error e -> code_from_type_error e
  | Other_error e -> code_from_other_error e

let error_name_from_code = function
  | 101 -> Some ("lexing", "unexpected character")
  | 102 -> Some ("lexing", "illegal escape sequence")
  | 103 -> Some ("lexing", "unterminated string")
  | 104 -> Some ("lexing", "invalid literal")
  | 105 -> Some ("lexing", "invalid builtin type")
  | 201 -> Some ("syntax", "expected token")
  | 202 -> Some ("syntax", "unexpected token")
  | 203 -> Some ("syntax", "unclosed token")
  | 204 -> Some ("syntax", "other syntax error")
  | 301 -> Some ("type", "expected a different type")
  | 302 -> Some ("type", "expected function type")
  | 303 -> Some ("type", "recursive type")
  | 304 -> Some ("type", "use of unbound symbol")
  | 305 -> Some ("type", "use of invalid magical")
  | 306 -> Some ("type", "module has no symbol")
  | 307 -> Some ("type", "cannot use private symbol")
  | 308 -> Some ("type", "cannot import from non-module symbol")
  | 309 -> Some ("type", "could not infer type")
  | 310 -> Some ("type", "other type error")
  | 401 -> Some ("other", "could not open file")
  | 402 -> Some ("other", "cannot import directory")
  | 403 -> Some ("other", "could not compile file")
  | 404 -> Some ("other", "failed to import symbol")
  | 405 -> Some ("other", "cannot explain error code")
  | 406 -> Some ("other", "nonexistent pass")
  | 407 -> Some ("other", "invalid opt level optimization level")
  | 408 -> Some ("other", "other error")
  | _ -> None

let get_code_opt = function
  | Other_error _ -> None
  | e -> Some (code_from_error e)

let explanation_from_code : int -> string option = function
  | 101 -> Some "This error is raised when the compiler's input contains an unexpected character, such as non-ascii characters."
  | 102 -> Some "This error is raised when a string or character literal contains an invalid escape sequence. Valid escape sequences include the following: \\a, \\b, \\f, \\n, \\r, \\t, \\v, \\\\, \\', \\\", \\0."
  | 103 -> Some "This error is raised when a string isn't terminated by a \"."
  | 104 -> Some "This error is raised when an integer, float or character literal contains invalid digits or an invalid character."
  | 105 -> Some "This error is raised when an invalid builtin type is encountered, such as \"$wrong\". Valid builtin types must one of the following: \"$i.(s|u).<int>\", \"$i.*\", \"$f.(64|32|16)\", \"f.*\" or \"$v\"."
  | 201 -> Some "This error is raised when a specific token was expected, but a different token was encountered. For example: the keyword \"in\" was expected, but the identifier \"im\" was found."
  | 202 -> Some "This error is raised when an unexpected token is encountered. This means that there is an invalid keyword or symbol."
  | 203 -> Some "This error is raised when a bracket, parenthesis or brace has not been closed. For example, when there's a \"(\", but no closing \")\"."
  | 204 -> Some "This error indicates a miscellaneous syntax error."
  | 301 -> Some "This error is raised when a specific type was expected but another type was found. This means that a value or function is of the wrong type."
  | 302 -> Some "This error is raised when a function type was expected, but another non-function type was found. This error occurs when, for example, it is attempted to call a non-function value."
  | 303 -> Some "This error indicates a recursive type, which is not supported."
  | 304 -> Some "This error is raised when a undefined type or variable is used. Often this is caused by a simple typo."
  | 305 -> Some "This error is raised when it is attempted to use an invalid magical operator, such as \"#wrong\"."
  | 306 -> Some "This error is raised when trying to import- or use a symbol that does not exist from a module or file."
  | 307 -> Some "This error is raised when it is attempted to import- or use a private symbol from a module. Note that symbols starting with an underscore are implicitly private to their module."
  | 308 -> Some "This error is raised when it is attempted to import a symbol from a value/symbol that isn't a module."
  | 309 -> Some "This error indicates that the type of a value could not be inferred. Usually this can be solved with type annotations."
  | 310 -> Some "This error indicates a miscellaneous type error."
  | 401 -> Some "This error is raised when a file could not be opened. Usually this is because the file in question does not exist."
  | 402 -> Some "This error is raised when it is attempted to import a directory. This is not supported."
  | 403 -> Some "This error is raised when compilation of a file failed due to previous errors."
  | 404 -> Some "This error is raised when importing a file failed due to errors originating from that file."
  | 405 -> Some "This error is raised when an invalid error code is passed with the `--explain` command-line option."
  | 406 -> Some "This error is raised when a nonexistent pass is passed with the `--pass` or `-p` command-line option."
  | 407 -> Some "This error is raised when an invalid optimization level is passed with the `-O` command-line option."
  | _ -> None
