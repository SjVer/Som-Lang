open Ast

type token_typ =
  (* keywords *)
  | LET   | IN
  | TYPE  | IS     | OF
  | EXT
  | USE   | FROM   | AS
  | MOD
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
  | MAGICNAME s | DIRECTNAME s
  | PRIMENAME s -> s
  | t -> failwith ("unpack_str " ^ show_token_typ t)

let unpack_lit = function
  | INTEGER i -> LIInt i
  | FLOAT f -> LIFloat f
  | CHARACTER c -> LIChar c
  | STRING s -> LIString s
  | EMPTYPARENS -> LINil
  | _ -> failwith "unpack_lit"

let unpack_typ = function
  | BUILTINITY a -> PTInt a
  | BUILTINFTY a -> PTFloat a
  | BUILTINVTY -> PTVoid
  | t -> failwith ("unpack_typ " ^ show_token_typ t)

let tokens_eq a b = without_arg a = without_arg b

let dummy = function
  | `UPPERNAME -> UPPERNAME ""
  | `LOWERNAME -> LOWERNAME ""
  | `PRIMENAME -> PRIMENAME ""
  | `DIRECTNAME -> DIRECTNAME ""
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