%{
open Ast
open Report.Error
open Span

let raise_error e = function
  | Some locs -> raise_error (Syntax_error e) (Some (span_from_lexlocs locs))
  | None -> raise_error (Syntax_error e) None

let unclosed what locs = raise_error (Unclosed what) (Some locs)

let mknode locs item = {span = span_from_lexlocs locs; item }
let mkbind patt expr = {patt; expr}

%}

// tokens

%token NOTEQUAL COLONEQUAL EQUAL
%token TRIPLEDOT DOUBLEDOT DOT
%token DOUBLECOLON COLON
%token COMMA
%token SEMICOLON

%token EMPTYPARENS LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE

%token DOUBLEBANG BANG
%token DOUBLEQUESTION QUESTION
%token ARROW
%token THICKARROW

%token DOUBLEPIPE PIPE
%token DOUBLEAMPERSAND AMPERSAND
%token DOUBLECARET CARET
%token STAR PLUS MINUS SLASH MODULO HASH

%token UNDERSCORE

%token <string> LOWERNAME
%token <string> UPPERNAME
%token <string> PRIMENAME

%token <bool> BOOL [@recover.expr false]
%token <int> INTEGER [@recover.expr 0]
%token <float> FLOAT [@recover.expr 0.0]
%token <char> CHARACTER [@recover.expr ' ']
%token <string> STRING [@recover.expr ""]

%token EOF

%start <Ast.expr node> prog

/*
  MAYBE-PROBLEM: definition vs let-binding?
  is there a syntaxical difference between them?
*/

%%

// rules

prog:
  | expr EOF { $1 }
;

// patterns

pattern:
  // TODO: rn the pattern's loc seems to be that of 
  // its expr instead (when coming from binding)
  | LOWERNAME { mknode $sloc (Variable $1) }
  | UNDERSCORE { mknode $sloc Wildcard }
;

// expressions

expr:
  | bindings THICKARROW seq { mknode $sloc (Binding ($1, $3)) }
  | seq { $1 }
;

bindings:
  | binding COMMA bindings { $1 :: $3 }
  | binding { [$1] }
;

(* the COMMA between bindings is overruled by `seq` so `term` instead? *)
binding: pattern EQUAL term { mkbind $1 $3 };

seq:
  | seq COMMA term { mknode $sloc (Sequence ($1, $3)) }
  | term { $1 }
;

term:
  | factor PLUS term { mknode $sloc (BinaryOp (Add, $1, $3)) }
  | factor MINUS term { mknode $sloc (BinaryOp (Subtract, $1, $3)) }
  | factor { $1 }
;

factor:
  | cast STAR factor { mknode $sloc (BinaryOp (Multiply, $1, $3)) }
  | cast SLASH factor { mknode $sloc (BinaryOp (Divide, $1, $3)) }
  | cast { $1 }
;

cast:
  | unary { $1 }
;

unary:
  | MINUS unary { mknode $sloc (UnaryOp (Negate, $2)) }
  | BANG unary { mknode $sloc (UnaryOp (Not, $2)) }
  | subscript { $1 }
;

subscript:
  | primary { $1 }
;

primary:
  | LPAREN expr RPAREN { mknode $sloc (Grouping $2) }
  | LPAREN expr error { unclosed "(" $loc($1) }

  | BOOL { mknode $sloc (Literal (Bool $1)) }
  | INTEGER { mknode $sloc (Literal (Int $1)) }
  | FLOAT { mknode $sloc (Literal (Float $1)) }
  | STRING { mknode $sloc (Literal (String $1)) }
  | CHARACTER { mknode $sloc (Literal (Char $1)) }

  | error { raise_error Expected_expression (Some $loc($1))}
;