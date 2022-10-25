open Ast

type token_typ =
  (* single *)
  | AT
  | EOF
  | HASH
  | UNDERSCORE
  | QUESTION
  | SEMICOLON
  | DOT
  | COMMA
  | COLON
  | BANG
  | BACKSLASH

  (* enclosing *)
  | RPAREN
  | LPAREN
  | RBRACKET
  | LBRACKET
  | RBRACE
  | LBRACE

  (* operators *)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | MODULO
  | CARET
  | AMPERSAND
  | PIPE
  | EQUAL
  | NOTEQUAL
  | GREATER
  | GREATEREQUAL
  | LESSER
  | LESSEREQUAL

  (* multiple *)
  | ARROW
  | DBL_PIPE
  | DBL_DOT
  | DBL_COLON
  | DBL_CARET
  | DBL_BANG
  | DBL_AMPERSAND
  | DBL_SEMICOLON
  | DBL_QUESTION
  | THICKARROW
  | TRP_DOT
  | EMPTYPARENS
  | COLONEQUAL

  (* ambigous *)
  | UPPERNAME of string
  | LOWERNAME of string
  | PRIMENAME of string
  | INTEGER of int
  | FLOAT of float
  | CHARACTER of char
  | STRING of string
  | BUILTINITY of (bool * int)
  | BUILTINFTY of int
  | BUILTINVTY
  [@@deriving show, eq]

let without_arg = function
  | UPPERNAME _ -> `UPPERNAME
  | LOWERNAME _ -> `LOWERNAME
  | PRIMENAME _ -> `PRIMENAME
  | INTEGER _ -> `INTEGER
  | FLOAT _ -> `FLOAT
  | CHARACTER _ -> `CHARACTER
  | STRING _ -> `STRING
  | BUILTINITY _ -> `BUILTINITY
  | BUILTINFTY _ -> `BUILTINFTY
  | t -> `OTHER t

let unpack_name = function
  | UPPERNAME s | LOWERNAME s
  | PRIMENAME s -> s
  | _ -> failwith "unpack_name"
let unpack_lit = function
  | INTEGER i -> LI_Int i
  | FLOAT f -> LI_Float f
  | CHARACTER c -> LI_Char c
  | STRING s -> LI_String s
  | EMPTYPARENS -> LI_Nil
  | _ -> failwith "unpack_lit"

let tokens_eq a b = without_arg a = without_arg b

let dummy = function
  | `UPPERNAME -> UPPERNAME ""
  | `LOWERNAME -> LOWERNAME ""
  | `PRIMENAME -> PRIMENAME ""
  | `INTEGER -> INTEGER 0
  | `FLOAT -> FLOAT 0.0
  | `CHARACTER -> CHARACTER '\x00'
  | `STRING -> STRING ""
  | `BUILTINITY -> BUILTINITY (false, 0)
  | `BUILTINFTY -> BUILTINFTY 0
  | `OTHER t -> t

type token =
  {
    typ: token_typ;
    span: Span.t;
  }