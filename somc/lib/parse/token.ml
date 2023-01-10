open Ast

type token_typ =
  (* keywords *)
  | USE   | FROM   | AS
  | MOD
  | TYPE  | IS     | OF
  | LET   | IN
  | IF    | THEN   | ELSE
  | FOR   | AT     | DO
  | MATCH | SWITCH

  (* single symbol *)
  | UNDERSCORE
  | SEMICOLON
  | DOT
  | COMMA
  | BANG
  | BACKSLASH
  | TILDE
  | PIPE

  (* enclosing *)
  | LPAREN   | RPAREN
  | LBRACKET | RBRACKET
  | LBRACE   | RBRACE

  (* operators *)
  | PLUS    | MINUS
  | STAR    | SLASH
  | MODULO
  | EQUAL   | NOTEQUAL
  | GREATER | GREATEREQUAL
  | LESSER  | LESSEREQUAL

  (* multiple *)
  | ARROW
  | DBL_DOT
  | TRP_DOT
  | COLON
  | DBL_COLON
  | DBL_PIPE
  | DBL_AMPERSAND
  | EMPTYPARENS
  
  (* ambigous *)
  | UPPERNAME of string
  | LOWERNAME of string
  | PRIMENAME of string
  | DIRECTNAME of string
  | EXTERNNAME of string
  | MAGICNAME of string
  | INTEGER of int
  | FLOAT of float
  | CHARACTER of char
  | STRING of string
  | BUILTINITY of (bool * int) option
  | BUILTINFTY of int option
  | BUILTINVTY

  (* misc *)
  | EOF
  [@@deriving show {with_path = false}, eq]
  
let without_arg = function
  | UPPERNAME _ -> `UPPERNAME
  | LOWERNAME _ -> `LOWERNAME
  | PRIMENAME _ -> `PRIMENAME
  | DIRECTNAME _ -> `DIRECTNAME
  | EXTERNNAME _ -> `EXTERNNAME
  | MAGICNAME _ -> `MAGICNAME
  | INTEGER _ -> `INTEGER
  | FLOAT _ -> `FLOAT
  | CHARACTER _ -> `CHARACTER
  | STRING _ -> `STRING
  | BUILTINITY _ -> `BUILTINITY
  | BUILTINFTY _ -> `BUILTINFTY
  | t -> `OTHER t

let unpack_str = function
  | UPPERNAME s | LOWERNAME s
  | EXTERNNAME s | MAGICNAME s
  | DIRECTNAME s
  | PRIMENAME s -> s
  | t -> failwith ("unpack_str " ^ show_token_typ t)

let unpack_lit = function
  | INTEGER i -> LI_Int i
  | FLOAT f -> LI_Float f
  | CHARACTER c -> LI_Char c
  | STRING s -> LI_String s
  | EMPTYPARENS -> LI_Nil
  | _ -> failwith "unpack_lit"

let unpack_typ = function
  | BUILTINITY a -> PT_Int a
  | BUILTINFTY a -> PT_Float a
  | BUILTINVTY -> PT_Void
  | t -> failwith ("unpack_typ " ^ show_token_typ t)

let tokens_eq a b = without_arg a = without_arg b

let dummy = function
  | `UPPERNAME -> UPPERNAME ""
  | `LOWERNAME -> LOWERNAME ""
  | `PRIMENAME -> PRIMENAME ""
  | `DIRECTNAME -> DIRECTNAME ""
  | `EXTERNNAME -> EXTERNNAME ""
  | `MAGICNAME -> MAGICNAME ""
  | `INTEGER -> INTEGER 0
  | `FLOAT -> FLOAT 0.0
  | `CHARACTER -> CHARACTER '\x00'
  | `STRING -> STRING ""
  | `BUILTINITY -> BUILTINITY None
  | `BUILTINFTY -> BUILTINFTY None
  | `OTHER t -> t

type token =
  {
    typ: token_typ;
    span: Span.t;
  }