%{
open Ast
open Report.Error
open Span


let raise_error e locs notes = raise_error (Syntax_error e) (Some (span_from_lexlocs locs)) notes

let unclosed s e what locs =
  raise_error (Unclosed s) locs
  [Printf.sprintf "try adding '%s' after the enclosed %s" what e]
let expected what locs = raise_error (Expected what) locs []

let mknode locs item = {span = span_from_lexlocs locs; item }
let mkbind patt expr = {patt; expr}
let mkimp path kind = {path; kind}
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

%token <bool> BOOL
%token <int> INTEGER
%token <float> FLOAT
%token <char> CHARACTER
%token <string> STRING

%token EOF

// ========================== precedence ==========================

%left PLUS MINUS
%left SLASH STAR
%right CARET
// %nonassoc ARROW
%nonassoc prec_unary

%left p

// =========================== helpers ===========================

%start <Ast.toplevel node list> prog
%%

%inline snl(s, X): separated_nonempty_list(s, X) { $1 }

// ============================ rules ============================

prog:
  | toplevel* EOF { $1 }
  | toplevel* error { expected "toplevel statement" $loc($2) }
;

// =========================== toplevel ===========================

toplevel:
  | import { $1 }
  | declaration { $1 }
  | definition { $1 }
;

import:
  | HASH import_body { mknode $sloc (TL_Import $2) }
  | HASH error { expected "a path" $loc($2) }
;

import_body:
  | snl(DOUBLECOLON, path_segment) noncolon_import_kind { mkimp $1 $2 }
  | list(path_segment DOUBLECOLON {$1}) colon_import_kind { mkimp $1 $2 }
;
// don't question the mess. accept that it (and nothing else) works.
%inline path_segment: LOWERNAME { mknode $sloc $1 };
%inline noncolon_import_kind:
  | /* empty */ { mknode $sloc IK_Simple }
  | THICKARROW UPPERNAME { mknode $sloc (IK_Rename $2) }
  | THICKARROW LOWERNAME { mknode $sloc (IK_Rename $2) }
;
%inline colon_import_kind:
  | STAR { mknode $sloc IK_Glob }

  | LBRACE snl(COMMA, import_body { mknode $sloc $1 }) RBRACE
    { mknode $sloc (IK_Nested $2) }
  | LBRACE snl(COMMA, import_body) error
    { unclosed "{" "}" "nested imports" $loc($3) }
;

declaration:
  | LOWERNAME COLON typ { mknode $sloc (TL_Declaration ($1, $3)) }
  | LOWERNAME COLON error { expected "a type" $loc($3) }
;

// TODO: the following is amb. 
// ```
//    f = x
//    g = y
// ```
// right now it is parsed as:
//    (decl f: x g)
//    (error `=`: "expected toplevel statement")
// solutions:
//    add prefix symbol before defs (e.g. "let")
//    add suffix symbol after defs/exprs (e.g. ";")
//    smth else?
definition:
  | pattern EQUAL expr { mknode $sloc (TL_Definition (mkbind $1 $3)) }
  | pattern EQUAL error { expected "an expression" $loc($3) }
;

// =========================== patterns ===========================

pattern:
  // TODO: rn the pattern's loc seems to be that of 
  // its expr instead (when coming from binding)
  | LOWERNAME { mknode $sloc (PA_Variable $1) }
  | UNDERSCORE { mknode $sloc PA_Wildcard }
;

// ========================== expressions ==========================

expr:
  | bindings THICKARROW seq_expr { mknode $sloc (EX_Binding ($1, $3)) }
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
  | seq_expr COMMA base_expr { mknode $sloc (EX_Sequence ($1, $3)) }
  | base_expr { $1 }
;

base_expr:
  | single_expr single_expr+
    { mknode $sloc (EX_Application (mknode $loc($1) (AP_Expr $1), $2)) }
  | base_expr infix_op base_expr
    { mknode $sloc (EX_Application (mknode $loc($2) $2, [$1; $3])) }
  | unary_op base_expr %prec prec_unary
    { mknode $sloc (EX_Application (mknode $loc($1) $1, [$2])) }
  // | base_expr ARROW typ { mknode $sloc (EX_Cast ($1, $3))}
  | single_expr { $1 }

  | error { raise_error Expected_expression $loc($1) [] }
;

%inline infix_op:
  | PLUS { AP_BinaryOp BI_Add }
  | MINUS { AP_BinaryOp BI_Subtract }
  | STAR { AP_BinaryOp BI_Multiply }
  | SLASH { AP_BinaryOp BI_Divide }
  | CARET { AP_BinaryOp BI_Power }
;
%inline unary_op:
  | MINUS { AP_UnaryOp UN_Negate }
  | BANG { AP_UnaryOp UN_Not }
;

single_expr:
  | LPAREN expr RPAREN { mknode $sloc (EX_Grouping $2) }
  | LPAREN expr error { unclosed "(" ")" "expression" $loc($1) }

  | BOOL { mknode $sloc (EX_Literal (LI_Bool $1)) }
  | INTEGER { mknode $sloc (EX_Literal (LI_Int $1)) }
  | FLOAT { mknode $sloc (EX_Literal (LI_Float $1)) }
  | STRING { mknode $sloc (EX_Literal (LI_String $1)) }
  | CHARACTER { mknode $sloc (EX_Literal (LI_Char $1)) }

  | variable { mknode $sloc $1 }
;

// literal-ish expression

%inline variable:
  | LOWERNAME { EX_Ident $1 }
;

// ============================= types =============================

typ:
  | alias_type { $1 }
  | error { raise_error Expected_type $loc($1) [] }
;

alias_type:
  | function_type { $1 }
  | alias_type THICKARROW PRIMENAME
    { mknode $sloc (TY_Alias ($1, mknode $loc($3) $3)) }
;

function_type:
  | function_type_args ARROW function_type { mknode $sloc (TY_Function ($1, $3)) }
  | tuple_type { $1 }
;

function_type_args:
  | tuple_type COMMA function_type_args { $1 :: $3 }
  | tuple_type { [$1] }
;

tuple_type:
  | snl(SEMICOLON, effect_type) {
    match $1 with [t] -> t
    | _ -> mknode $sloc (TY_Tuple ($1))
  }
  | error { raise_error Expected_type $loc($1) [] }
;

effect_type:
  | atomic_type { $1 }
  | BANG atomic_type { mknode $sloc (TY_Effect (Some $2)) }
  | BANG { mknode $sloc (TY_Effect None) }
;

atomic_type:
  | LPAREN typ RPAREN { mknode $sloc (TY_Grouping $2) }
  | LPAREN typ error { unclosed "(" ")" "type" $loc($1) }

  | LBRACKET typ RBRACKET { mknode $sloc (TY_List $2) }
  | LBRACKET typ error { unclosed "[" "]" "list type" $loc($1) }

  | BUILTINITY { let (s, w) = $1 in mknode $sloc (TY_Builtin (BT_Int (s, w))) }
  | BUILTINFTY { mknode $sloc (TY_Builtin (BT_Float $1)) }
  | BUILTINVTY { mknode $sloc (TY_Builtin BT_Void) }

  | UNDERSCORE { mknode $sloc TY_Any }
  | PRIMENAME { mknode $sloc (TY_Var $1) }
  | atomic_type? UPPERNAME { mknode $sloc (TY_Constr ($1, $2)) }
;