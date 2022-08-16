%{
open Ast
open Report.Error
open Span

let raise_error e = function
  | Some locs -> raise_error (Syntax_error e) (Some (span_from_lexlocs locs))
  | None -> raise_error (Syntax_error e) None

let unclosed what locs = raise_error (Unclosed what) (Some locs)

let mkexpr locs item = {span = span_from_lexlocs locs; item }

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

%token <string> LOWERNAME
%token <string> UPPERNAME
%token <string> PRIMENAME

%token <bool> BOOL [@recover.expr false]
%token <int> INTEGER [@recover.expr 0]
%token <float> FLOAT [@recover.expr 0.0]
%token <char> CHARACTER [@recover.expr ' ']
%token <string> STRING [@recover.expr ""]

%token EOF

// precedence
%left PLUS MINUS
%left STAR SLASH
// %nonassoc NEGATE

%start <Ast.expr node> prog

%%

// rules

prog:
  | expr EOF { $1 }
;

expr:
  | term { $1 }

term:
  | factor PLUS term { mkexpr $sloc (BinaryOp (Add, $1, $3)) }
  | factor MINUS term { mkexpr $sloc (BinaryOp (Subtract, $1, $3)) }
  | factor { $1 }
;

factor:
  | cast STAR factor { mkexpr $sloc (BinaryOp (Multiply, $1, $3)) }
  | cast SLASH factor { mkexpr $sloc (BinaryOp (Divide, $1, $3)) }
  | cast { $1 }
;

cast:
  | unary { $1 }
;

unary:
  | MINUS unary { mkexpr $sloc (UnaryOp (Negate, $2)) }
  | BANG unary { mkexpr $sloc (UnaryOp (Not, $2)) }
  | subscript { $1 }
;

subscript:
  | primary { $1 }
;

primary:
  | LPAREN expr RPAREN { mkexpr $sloc (Grouping $2) }
  | LPAREN expr error { unclosed "(" $loc($1) }

  | BOOL { mkexpr $sloc (Literal (Bool $1)) }
  | INTEGER { mkexpr $sloc (Literal (Int $1)) }
  | FLOAT { mkexpr $sloc (Literal (Float $1)) }
  | STRING { mkexpr $sloc (Literal (String $1)) }
  | CHARACTER { mkexpr $sloc (Literal (Char $1)) }

  | error { raise_error Expected_expression (Some $loc($1))}
;