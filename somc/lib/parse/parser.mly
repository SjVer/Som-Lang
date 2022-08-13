%{
open Ast
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

%token <bool> BOOL
%token <string> LOWERNAME
%token <string> UPPERNAME
%token <string> INTEGER
%token <string> FLOAT
%token <char list> CHARACTER
%token <char list> STRING

%token EOF

// precedence
// %left PLUS MINUS
// %left STAR SLASH
%nonassoc NEGATE

%start <Ast.expr_node> prog

%%

// rules

prog:
  | e = expr EOF { e }
;

expr:
  | e = expr PLUS t = term { BinaryOp (Add, e, t) }
  | e = expr MINUS t = term { BinaryOp (Subtract, e, t) }
  | t = term { t }
;

term:
  | MINUS t = term %prec NEGATE { Negate t }
  | t = term STAR f = factor { BinaryOp (Multiply, t, f) }
  | t = term SLASH f = factor { BinaryOp (Divide, t, f) }
  | f = factor { f }
;

factor:
  | f1 = factor CARET f2 = factor { BinaryOp (Power, f1, f2) }
  | LPAREN e = expr RPAREN { Grouping e }
  | n = BOOL { Literal (Bool n) }
  | n = INTEGER { Literal (Int n) }
  | n = FLOAT { Literal (Float n) }
;