(* File generated by gen_codes_ml.py on Sun Aug 21 15:03:44 2022. *)

open Error

let int_from_lexing_error = function
  | Unexpected_character _ -> 101
  | Illegal_escape _ -> 102
  | Unterminated_string -> 103
  | Invalid_literal _ -> 104
  | Invalid_builtin_type _ -> 105

let int_from_syntax_error = function
  | Expected _ -> 201
  | Unexpected -> 202
  | Expected_toplevel -> 203
  | Expected_expression -> 204
  | Expected_type -> 205
  | Unclosed _ -> 206
  | Use_of_unbound _ -> 207
  | Duplicate_parameter _ -> 208
  | Other _ -> 209

let int_from_other_error = function
  | Could_not_open _ -> 301
  | Could_not_compile _ -> 302
  | Failed_to_resolve _ -> 303
  | Failed_to_import _ -> 304
  | Cannot_explain _ -> 305

let int_from_error = function
  | Lexing_error e -> int_from_lexing_error e
  | Syntax_error e -> int_from_syntax_error e
  | Other_error e -> int_from_other_error e

let error_name_from_int = function
  | 101 -> Some "unexpected character"
  | 102 -> Some "illegal escape sequence"
  | 103 -> Some "unterminated string"
  | 104 -> Some "invalid literal"
  | 105 -> Some "invalid builtin type"
  | 201 -> Some "expected token"
  | 202 -> Some "unexpected token"
  | 203 -> Some "expected toplevel"
  | 204 -> Some "expected expression"
  | 205 -> Some "expected type"
  | 206 -> Some "unclosed token"
  | 207 -> Some "use of unbound symbol"
  | 208 -> Some "duplicate parameter"
  | 209 -> Some "other syntax error"
  | 301 -> Some "could not open file"
  | 302 -> Some "could not compile file"
  | 303 -> Some "failed to resolve module"
  | 304 -> Some "failed to import module"
  | 305 -> Some "cannot explain error code"
  | _ -> None