%{
open Ast
open Report.Error
open Span


let raise_error e locs notes = raise_error (Syntax_error e) (Some (span_from_lexlocs locs)) notes

let unclosed s e locs =
  raise_error (Unclosed s) locs
  [Printf.sprintf "try adding '%s' after the enclosed code" e]

let mknode locs item = {span = span_from_lexlocs locs; item }
let mkbind patt expr = {patt; expr}
%}

// ============================ tokens ============================

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
%token <bool * int> BUILTINITY
%token <int> BUILTINFTY
%token BUILTINVTY

%token <string> LOWERNAME
%token <string> UPPERNAME
%token <string> PRIMENAME

%token <bool> BOOL [@recover.expr false]
%token <int> INTEGER [@recover.expr 0]
%token <float> FLOAT [@recover.expr 0.0]
%token <char> CHARACTER [@recover.expr ' ']
%token <string> STRING [@recover.expr ""]

%token EOF

// ========================== precedence ==========================

%left PLUS MINUS
%left SLASH STAR
%right CARET
%nonassoc ARROW
%nonassoc prec_unary

// ============================ rules ============================

%start <Ast.expr node> prog
%%

prog:
  | expr EOF { $1 }
  // | expr error { expected "EOF" $loc($2) }
;

// =========================== patterns ===========================

pattern:
  // TODO: rn the pattern's loc seems to be that of 
  // its expr instead (when coming from binding)
  | LOWERNAME { mknode $sloc (P_Variable $1) }
  | UNDERSCORE { mknode $sloc P_Wildcard }
;

// ========================== expressions ==========================

expr:
  | bindings THICKARROW seq_expr { mknode $sloc (E_Binding ($1, $3)) }
  | seq_expr { $1 }

  | error { raise_error Expected_expression $loc($1) [] }
;

bindings:
  | binding COMMA bindings { $1 :: $3 }
  | binding { [$1] }
;
%inline binding: pattern EQUAL base_expr { mkbind $1 $3 };

// base expression (sequences and applications)

seq_expr:
  | seq_expr COMMA base_expr { mknode $sloc (E_Sequence ($1, $3)) }
  | base_expr { $1 }
;

base_expr:
  | single_expr single_expr+ { mknode $sloc (E_Application (mknode $loc($1) (A_Expr $1), $2)) }
  | base_expr infix_op base_expr { mknode $sloc (E_Application (mknode $loc($2) $2, [$1; $3])) }
  | base_expr ARROW type_ { mknode $sloc (E_Cast ($1, $3))}
  | unary_op base_expr %prec prec_unary { mknode $sloc (E_Application (mknode $loc($1) $1, [$2])) }
  | single_expr { $1 }

  | error { raise_error Expected_expression $loc($1) [] }
;

%inline infix_op:
  | PLUS { A_BinaryOp B_Add }
  | MINUS { A_BinaryOp B_Subtract }
  | STAR { A_BinaryOp B_Multiply }
  | SLASH { A_BinaryOp B_Divide }
  | CARET { A_BinaryOp B_Power }
;
%inline unary_op:
  | MINUS { A_UnaryOp U_Negate }
  | BANG { A_UnaryOp U_Not }
;

single_expr:
  | LPAREN expr RPAREN { mknode $sloc (E_Grouping $2) }
  | LPAREN expr error { unclosed "(" ")" $loc($1) }

  | BOOL { mknode $sloc (E_Literal (L_Bool $1)) }
  | INTEGER { mknode $sloc (E_Literal (L_Int $1)) }
  | FLOAT { mknode $sloc (E_Literal (L_Float $1)) }
  | STRING { mknode $sloc (E_Literal (L_String $1)) }
  | CHARACTER { mknode $sloc (E_Literal (L_Char $1)) }

  | variable { mknode $sloc $1 }
;

// literal-ish expression

%inline variable:
  | LOWERNAME { E_Ident $1 }
;

// ============================= types =============================

type_:
  | alias_type { $1 }
  | error { raise_error Expected_type $loc($1) [] }
;

alias_type:
  | function_type { $1 }
  | alias_type COLONEQUAL PRIMENAME { mknode $sloc (T_Alias ($1, mknode $loc($3) $3)) }
;

function_type:
  | tuple_type %prec ARROW { $1 }
  | function_type ARROW tuple_type { mknode $sloc (T_Function ($1, $3)) }
;

tuple_type:
  | atomic_type { $1 }
  | error { raise_error Expected_type $loc($1) [] }
;

atomic_type:
  | LPAREN type_ RPAREN { mknode $sloc (T_Grouping $2) }
  | LPAREN type_ error { unclosed "(" ")" $loc($1) }

  | BUILTINITY { let (s, w) = $1 in mknode $sloc (T_Builtin (T_B_Int (s, w))) }
  | BUILTINFTY { mknode $sloc (T_Builtin (T_B_Float $1)) }
  | BUILTINVTY { mknode $sloc (T_Builtin T_B_Void) }

  | UNDERSCORE { mknode $sloc T_Any }
  | PRIMENAME { mknode $sloc (T_Var $1) }
  | type_params UPPERNAME { mknode $sloc (T_Constr ($2, $1)) }
;

%inline type_params:
  | /* empty */ { [] }
  | atomic_type { [$1] }
  // TODO: parse multiple args: `(T1; ...; Tn)`
;