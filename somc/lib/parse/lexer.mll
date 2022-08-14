{
open Parser

type state = {
  raw_buffer : Buffer.t;
  txt_buffer : Buffer.t;
}

let get_scratch_buffers { raw_buffer; txt_buffer } =
  Buffer.reset raw_buffer;
  Buffer.reset txt_buffer;
  ( raw_buffer, txt_buffer )

(* Specialize raise_error for lexing errors *)
let raise_error loc error = raise_error (Lexing_error error) loc

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then (
    raise_error
        (Location.curr lexbuf)
        (Illegal_escape (Lexing.lexeme lexbuf));
    'x'
  ) else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

}

let prime = '\''
let alpha = ['a'-'z' 'A'-'Z' '0'-'9']
let digit = ['0'-'9']

let term_name = (['a'-'z'] alpha* prime*)
let type_name = (['A'-'Z'] alpha* prime*)

let b_int = '0' ('b'|'B') ('0' | '1')+
let o_int = '0' ('c'|'C') ['0'-'7']+
let h_int = '0' ('x'|'X') ['0'-'9' 'a'-'f' 'A'-'F']+
let int = digit+ | b_int | o_int | h_int
let float = (digit+ '.' digit+)

let blank = [' ' '\t']

rule main = parse
  | blank { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | "---" { block_comment lexbuf; main lexbuf }
  | "--" { simple_comment lexbuf; main lexbuf }

  | "/=" { NOTEQUAL }
  | ":=" { COLONEQUAL }
  | '=' { EQUAL }

  | "..." { TRIPLEDOT }
  | ".." { DOUBLEDOT }
  | '.' { DOT }

  | "::" { DOUBLECOLON }
  | ':' { COLON }
  | ',' { COMMA }
  | ';' { SEMICOLON }

  | "()" { EMPTYPARENS }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }

  | "!!" { DOUBLEBANG }
  | '!' { BANG }
  | "??" { DOUBLEQUESTION }
  | '?' { QUESTION }
  | "->" { ARROW }
  | "=>" { THICKARROW }

  | "||" { DOUBLEPIPE }
  | '|' { PIPE }
  | "&&" { DOUBLEAMPERSAND }
  | '&' { AMPERSAND }
  | "^^" { DOUBLECARET }
  | '^' { CARET }
  | '*' { STAR }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { SLASH }
  | '%' { MODULO }
  | '#' { HASH }

  | "true" { BOOL true }
  | "false" { BOOL false }
  | term_name as name { LOWERNAME name }
  | type_name as name { UPPERNAME name }
  | int as n { INTEGER (int_of_string n) }
  | float as n { FLOAT (float_of_string n) }

  | "'" { lex_string }

  | '"' {
    let string_start = lexbuf.lex_start_p in
    let start_loc = Location.curr lexbuf in
    let raw_buffer, txt_buffer = get_scratch_buffers state in
    if not (string raw_buffer (Some txt_buffer) lexbuf) then
      raise_error start_loc Unterminated_string;
    lexbuf.lex_start_p <- string_start;
    let txt = flush_buffer txt_buffer in
    let raw = flush_buffer raw_buffer in
    STRING (txt, Some raw, None)
  }

  | eof { EOF }
  | _ { raise Error }

and simple_comment = parse
  | '\n' { Lexing.new_line lexbuf }
  | eof { () }
  | _ { simple_comment lexbuf }

and block_comment = parse
  | '\n' { Lexing.new_line lexbuf; block_comment lexbuf }
  | "---" { () }
  | eof { () }
  | _ { block_comment lexbuf }

and get_char = parse
  | "\\'" { '\'' :: get_char lexbuf }
  | '\'' { [] }
  | _ as c { c :: get_char lexbuf }
  | eof { raise Error }