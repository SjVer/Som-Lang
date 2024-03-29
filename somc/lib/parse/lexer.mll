{
open Token
open Report.Error
open Span

let error span e notes =
  Report.make (`Error (Lexing_error e)) (Some span) notes []
  |> Report.report

let curr_span lexbuf = 
  let s = Lexing.lexeme_start_p lexbuf in
  let e = Lexing.lexeme_end_p lexbuf in
  span_from_lexlocs (s, e) false

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

let set_start_loc lexbuf loc =
  lexbuf.Lexing.lex_start_p <- loc

let kw_from_str = function
  | "use" -> USE
  | "from" -> FROM
  | "as" -> AS
  | "mod" -> MOD
  | "type" -> TYPE
  | "ext" -> EXT
  | "is" -> IS 
  | "of" -> OF 
  | "let" -> LET 
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "for" -> FOR
  | "at" -> AT
  | "do" -> DO
  | "match" -> MATCH
  | "switch" -> SWITCH
  | "end" -> END
  | "lam" -> LAM
  | _ -> raise Not_found

(* string stuff *)
let string_buff = Buffer.create 256
let reset_string_buffer () = Buffer.clear string_buff
let store_string_char c = Buffer.add_char string_buff c
let get_stored_string () = Buffer.contents string_buff

let char_for_backslash = function
  | 'a' -> '\x08'
  | 'b' -> '\b'
  | 'f' -> '\x0C'
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | 'v' -> '\x0B'
  | '0' -> '\x00'
  | c   -> c
}

let prime = '\''
let alpha = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let digit = ['0'-'9']

let operator_chars =
  [
    '$'  '&'  '@'  '+'  '*'  '-'  '='  '>'  '<'  '?'  ':'
    '!'  '.'  '%'  '~'  '|'  '/'  '['  ']'  '~' '^'
  ]

let lower_name = '_'* ['a'-'z'] alpha*
let upper_name = '_'* ['A'-'Z'] alpha*

let b_int = '0' ('b'|'B') ('0' | '1')+
let o_int = '0' ('c'|'C') ['0'-'7']+
let h_int = '0' ('x'|'X') ['0'-'9' 'a'-'f' 'A'-'F']+
let int = digit+ | b_int | o_int | h_int
let float = (digit+ '.' digit+)

let blank = ' ' | '\t' | '\r'
let backslash_escapes = ['\\' '\'' '"' 'a' 'b' 'f' 'n' 'r' 't' 'v' '0']

rule lex = parse
  | blank { lex lexbuf }
  | '\n' { Lexing.new_line lexbuf; lex lexbuf }
  | "---" { block_comment lexbuf; lex lexbuf }
  | "--" { simple_comment lexbuf; lex lexbuf }

  | "#" (lower_name as ident) { MAGICNAME ident }
  | "#" (upper_name as ident) { MAGICTYPE ident }
  | "!!" (lower_name as ident) { DIRECTNAME ident }

  | "/=" { NOTEQUAL }
  | '=' { EQUAL }

  | ".." { DBL_DOT }
  | '.' { DOT }

  | "::" { DBL_COLON }
  | ":" { COLON }
  | ';' { SEMICOLON }
  | ',' { COMMA }

  | '(' blank* (operator_chars+ as op) blank* ')' { LOWERNAME op }

  | "()" { EMPTYPARENS }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }

  | "->" { ARROW }
  | "<=" { LESSEREQUAL }
  | '<' { LESSER }
  | ">=" { GREATEREQUAL }
  | '>' { GREATER }

  | "||" { DBL_PIPE }
  | '|' { PIPE }
  | "&&" { DBL_AMPERSAND }
  | '*' { STAR }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { SLASH }
  | '%' { MODULO }
  | '!' { BANG }
  | '~' { TILDE }

  | ['*' '/' '%'] operator_chars+ as op { INFIX_OP_1 op }
  | ['&' '|'] operator_chars* as op { INFIX_OP_1 op } (* TODO: is this shit? *)
  | ['+' '-'] operator_chars+ as op { INFIX_OP_2 op }
  | ['<' '>'] operator_chars+ as op { INFIX_OP_3 op }
  | ("=" | "/=") operator_chars+ as op { INFIX_OP_4 op }
  | ['!' '~'] operator_chars+ as op { UNARY_OP op }

  | '"' {
    let start_loc = lexbuf.lex_start_p in
    reset_string_buffer ();
    string lexbuf;
    set_start_loc lexbuf start_loc;
    STRING (get_stored_string ())
  }
  | "'" ([^ '\\'] as c) "'"
    { CHARACTER c }
  | "'" '\\' backslash_escapes "'"
    { CHARACTER (char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'" '\\' (_ as c) {
    error (curr_span lexbuf) (Illegal_escape c) [];
    CHARACTER c
  }

  | int as n { INTEGER (int_of_string n) }
  | float as n { FLOAT (float_of_string n) }

  | (lower_name prime*) as name { try kw_from_str name with Not_found -> LOWERNAME name }
  | (upper_name prime*) as name { UPPERNAME name }
  | "'" (lower_name as ident) { PRIMENAME ident }

  | '_' { UNDERSCORE }

  | eof { EOF }
  | _ {
    error
      (curr_span lexbuf)
      (Unexpected_character (Lexing.lexeme lexbuf)) [];
    EOF
  }

and simple_comment = parse
  | '\n' { Lexing.new_line lexbuf }
  | eof { () }
  | _ { simple_comment lexbuf }

and block_comment = parse
  | '\n' { Lexing.new_line lexbuf; block_comment lexbuf }
  | "---" { () }
  | eof { () }
  | _ { block_comment lexbuf }

and string = parse
  | '"' { () }
  (* allow multi-line strings that escape the newline *)
  | '\\' ("\n" | "\r" | "\r\n") ([' ' '\t'] * as spaces) {
    incr_loc lexbuf (String.length spaces);
    string lexbuf
  }
  | '\\' (backslash_escapes as c) {
    store_string_char (char_for_backslash c);
    string lexbuf
  }
  | '\\' (_ as c) {
    error (curr_span lexbuf) (Illegal_escape c) [];
    store_string_char '\\';
    store_string_char c;
    string lexbuf
  }
  | eof { error (curr_span lexbuf) Unterminated_string [] }
  | '\n' {
    store_string_char '\n';
    incr_loc lexbuf 0;
    string lexbuf
  }
  | _ as c {
    store_string_char c;
    string lexbuf
  }

{
let get_tokens lexbuf =
  let mk t = {
      typ = t;
      span = span_from_lexbuf lexbuf false;
    }
  in
  let rec go acc =
    let t = mk (lex lexbuf) in
    match t.typ with
      | EOF -> t :: acc
      | _ -> go (t :: acc)
  in
  List.rev (go [])
}
