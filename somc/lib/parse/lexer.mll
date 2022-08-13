{
open Parser
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
  | int as n { INTEGER n }
  | float as n { FLOAT n }
  | '\'' { CHARACTER (get_char lexbuf) }
  | '"' { STRING (get_string lexbuf) }

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

and get_string = parse
  | "\\\"" { '\"' :: get_string lexbuf }
  | '"' { [] }
  | _ as c { c :: get_string lexbuf }
  | eof { raise Error }

{ end }