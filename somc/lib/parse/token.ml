open Ast

type token_typ =
  (* keywords *)
  | LET   | IN     | LAM
  | TYPE  | IS     | OF
  | EXT
  | USE   | FROM   | AS
  | MOD
  | IF    | THEN   | ELSE
  | FOR   | AT     | DO
  | MATCH | SWITCH | END

  (* single symbol *)
  | UNDERSCORE
  | SEMICOLON
  | DOT
  | COMMA
  | BANG
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
  
  (* operators *)
  | INFIX_OP_1 of string
  | INFIX_OP_2 of string
  | INFIX_OP_3 of string
  | INFIX_OP_4 of string
  | INFIX_OP_5 of string
  | INFIX_OP_6 of string
  | UNARY_OP of string

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

  (* misc *)
  | EOF
  [@@deriving show {with_path = false}, eq]
  
let without_arg = function
  | INFIX_OP_1 _ -> `INFIX_OP_1
  | INFIX_OP_2 _ -> `INFIX_OP_2
  | INFIX_OP_3 _ -> `INFIX_OP_3
  | INFIX_OP_4 _ -> `INFIX_OP_4
  | INFIX_OP_5 _ -> `INFIX_OP_5
  | INFIX_OP_6 _ -> `INFIX_OP_6
  | UNARY_OP _ -> `UNARY_OP
  | UPPERNAME _ -> `UPPERNAME
  | LOWERNAME _ -> `LOWERNAME
  | PRIMENAME _ -> `PRIMENAME
  | DIRECTNAME _ -> `DIRECTNAME
  | MAGICNAME _ -> `MAGICNAME
  | INTEGER _ -> `INTEGER
  | FLOAT _ -> `FLOAT
  | CHARACTER _ -> `CHARACTER
  | STRING _ -> `STRING
  | t -> `OTHER t

let unpack_str = function
  | INFIX_OP_1 s | INFIX_OP_2 s | INFIX_OP_3 s
  | INFIX_OP_4 s | INFIX_OP_5 s | INFIX_OP_6 s
  | UNARY_OP s
  | UPPERNAME s | LOWERNAME s
  | MAGICNAME s | DIRECTNAME s
  | PRIMENAME s -> s
  | t -> failwith ("unpack_str " ^ show_token_typ t)

let unpack_lit = function
  | INTEGER i -> Pli_int i
  | FLOAT f -> Pli_float f
  | CHARACTER c -> Pli_char c
  | STRING s -> Pli_string s
  | EMPTYPARENS -> Pli_null
  | _ -> failwith "unpack_lit"

let tokens_eq a b = without_arg a = without_arg b

let dummy = function
  | `INFIX_OP_1 -> INFIX_OP_1 ""
  | `INFIX_OP_2 -> INFIX_OP_2 ""
  | `INFIX_OP_3 -> INFIX_OP_3 ""
  | `INFIX_OP_4 -> INFIX_OP_4 ""
  | `INFIX_OP_5 -> INFIX_OP_5 ""
  | `INFIX_OP_6 -> INFIX_OP_6 ""
  | `UNARY_OP -> UNARY_OP ""
  | `UPPERNAME -> UPPERNAME ""
  | `LOWERNAME -> LOWERNAME ""
  | `PRIMENAME -> PRIMENAME ""
  | `DIRECTNAME -> DIRECTNAME ""
  | `MAGICNAME -> MAGICNAME ""
  | `INTEGER -> INTEGER 0
  | `FLOAT -> FLOAT 0.0
  | `CHARACTER -> CHARACTER '\x00'
  | `STRING -> STRING ""
  | `OTHER t -> t

type token =
  {
    typ: token_typ;
    span: Span.t;
  }