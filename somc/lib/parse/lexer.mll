{
open Parser
open Report.Error
open Span

let raise_error span e =
  raise_error (Lexing_error e) (Some span)

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

let lower_name = '_'* ['a'-'z'] alpha*
let upper_name = '_'* ['A'-'Z'] alpha*

let b_int = '0' ('b'|'B') ('0' | '1')+
let o_int = '0' ('c'|'C') ['0'-'7']+
let h_int = '0' ('x'|'X') ['0'-'9' 'a'-'f' 'A'-'F']+
let int = digit+ | b_int | o_int | h_int
let float = (digit+ '.' digit+)

let blank = ' ' | '\t' | '\r'
let backslash_escapes = ['\\' '\'' '"' 'a' 'b' 'f' 'n' 'r' 't' 'v' '0']

rule main = parse
  | blank { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | "---" { block_comment lexbuf; main lexbuf }
  | "--" { simple_comment lexbuf; main lexbuf }

  | "/=" { NOTEQUAL }
  | ":=" { COLONEQUAL }
  | '=' { EQUAL }

  | "..." { TRP_DOT }
  | ".." { DBL_DOT }
  | '.' { DOT }

  | "::" { DBL_COLON }
  | ':' { COLON }
  | ',' { COMMA }
  | ";;" { DBL_SEMICOLON }
  | ';' { SEMICOLON }

  | "()" { EMPTYPARENS }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }

  | "!!" { DBL_BANG }
  | '!' { BANG }
  | "??" { DBL_QUESTION }
  | '?' { QUESTION }

  | "->" { ARROW }
  | "=>" { THICKARROW }
  | '#' { HASH }
  | '@' { AT }
  | '\\' { BACKSLASH }

  | "<=" { LESSEREQUAL }
  | '<' { LESSER }
  | ">=" { GREATEREQUAL }
  | '>' { GREATER }

  | "||" { DBL_PIPE }
  | '|' { PIPE }
  | "&&" { DBL_AMPERSAND }
  | '&' { AMPERSAND }
  | "^^" { DBL_CARET }
  | '^' { CARET }
  | '*' { STAR }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { SLASH }
  | '%' { MODULO }

  | "$i." ('s'|'u' as s) '.' (digit+ as w) { BUILTINITY (s = 's', int_of_string w) }
  | "$f." ("64"|"32"|"16" as w) { BUILTINFTY (int_of_string w) }
  | "$v" { BUILTINVTY }
  | "$" (alpha | '.')* alpha {
    raise_error (curr_span lexbuf) (Invalid_builtin_type (Lexing.lexeme lexbuf))
    ["a valid builtin type is one of the following:\n$i.(s|u).<int>, $f.(64|32|16), $v"]
  }

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
  | "'" '\\' (_ as c)
    { raise_error (curr_span lexbuf) (Illegal_escape c) [] }

  | "true" { BOOL true }
  | "false" { BOOL false }
  | int as n { INTEGER (int_of_string n) }
  | float as n { FLOAT (float_of_string n) }

  | (lower_name prime*) as name { LOWERNAME name }
  | upper_name as name { UPPERNAME name }
  | "'" (lower_name as ident) { PRIMENAME ident }

  | '_' { UNDERSCORE }

  | eof { EOF }
  | _ { raise_error (curr_span lexbuf) (Unexpected_character (Lexing.lexeme lexbuf)) [] }

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
    raise_error (curr_span lexbuf) (Illegal_escape c) []
    (* warning lexbuf (Printf.sprintf "illegal backslash escape in string: `\\%c'" c); *)
    (* store_string_char '\\'; *)
    (* store_string_char c; *)
    (* string lexbuf *)
  }
  | eof { raise_error (curr_span lexbuf) Unterminated_string [] }
  | '\n' {
    store_string_char '\n';
    incr_loc lexbuf 0;
    string lexbuf
  }
  | _ as c {
    store_string_char c;
    string lexbuf
  }