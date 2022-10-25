%{
open Ast
open Report.Error
open Span

let raise_error e locs notes =
  let s = span_from_lexlocs locs false in
  Report.make (`Error (Syntax_error e)) (Some s) notes []
  |> Report.raise

let unclosed s e what locs =
  raise_error (Unclosed s) locs
  [Printf.sprintf "try adding '%s' after the enclosed %s." e what]
let expected what locs = raise_error (Expected what) locs []

let mknode locs item = {span = span_from_lexlocs locs false; item }
let mkgnode locs item = {span = span_from_lexlocs locs true; item }
let mkbind patt expr = {patt; expr}
let mkimp dir path kind = {dir; path; kind}
let mktypdecl name params typ = {name; params; typ}
let mkgop locs name = 
  let identif = Ident.from_list ["_std_ops"; name]
  in EX_Identifier (mkgnode locs identif)

let nil_constr = Ident.from_list ["_std_list"; "Nil"]
let cons_constr = Ident.from_list ["_std_list"; "Cons"]

let rec mkglist endlocs = function
  | [] -> 
    let empty = mkgnode endlocs nil_constr
    in mkgnode endlocs (EX_Construct (empty, None))
  | (l, e) :: es ->
    let cons = mkgnode l cons_constr in
    let cons_tuple = EX_Tuple [e; mkglist endlocs es] in
    let expr = EX_Construct (cons, Some (mkgnode l cons_tuple)) in
    mkgnode l expr

(** if set to true, grouping nodes will be produced
    otherwise the encased nodes will be passed through *)
let grp raw group = if false then group else raw
%}
// let mkdir id arg = {id; arg}

/*
  KNOWN CONFLICTS

  ** Conflict (shift/reduce) in state 195.
    ** Token involved: error
    pattern EQUAL pattern EQUAL LBRACKET
    >> f = g = [ !error
    :: to what rule does the error token belong?

  ** Conflict (shift/reduce) in state 165.
    ** Token involved: error
    pattern EQUAL base_expr SEMICOLON
    >> f = 1 ; !error
    :: to what rule does the error token belong?

  ** Conflict (shift/reduce) in state 106.
    ** Token involved: error
    pattern EQUAL LBRACKET
    >> f = [ !error
    :: to what rule does the error token belong?

  ** Conflict (reduce/reduce) in state 105.
    ** Tokens involved: LOWERNAME EQUAL
    pattern EQUAL LOWERNAME
    >> f = x?
    :: pattern vs application ambigouity

  ** Conflict (reduce/reduce) in state 99.
    ** Tokens involved: error RPAREN DOT
    pattern EQUAL error
    >> f = !error
    :: does the error belong to expr or base_expr?

  ** Conflict (shift/reduce) in state 39.
    ** Token involved: error
    LOWERNAME COLON effect_type SEMICOLON
    >> f : T ; !error
    :: does the error belong to effect_type or tuple_type?

*/

// ============================ tokens ============================

%token NOTEQUAL COLONEQUAL EQUAL
%token TRP_DOT DBL_DOT DOT
%token DBL_COLON COLON
%token COMMA
%token DBL_SEMICOLON SEMICOLON
 
%token EMPTYPARENS LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE

%token DBL_BANG BANG
%token DBL_QUESTION QUESTION
%token ARROW THICKARROW
%token BACKSLASH HASH AT

%token DBL_PIPE PIPE
%token DBL_AMPERSAND AMPERSAND
%token DBL_CARET CARET
%token GREATER GREATEREQUAL LESSER LESSEREQUAL
%token STAR PLUS MINUS SLASH MODULO

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

%left DBL_PIPE
%left DBL_CARET
%left DBL_AMPERSAND
%left EQUAL NOTEQUAL
%left GREATER GREATEREQUAL LESSER LESSEREQUAL
%left PLUS MINUS
%left SLASH STAR MODULO
%right CARET
%nonassoc prec_unary

// =========================== helpers ===========================

%start <Ast.ast> prog
%%

// separated non-empty list
%inline snel(SEP, X): separated_nonempty_list(SEP, X) { $1 }

// separated non-trivial list (at least 2 elements)
sntl(SEP, X):
  | sntl(SEP, X) SEP X { $1 @ [$3] }
  | X SEP X { [$1; $3] }
;

// separated semi-non-trivial list (at least 1 element but with SEP at end)
ssntl(SEP, X):
  | sntl(SEP, X) SEP X { $1 @ [$3] }
  | X SEP X { [$1; $3] }
  | X SEP { [$1] }
;

// ============================ rules ============================

prog:
  | toplevel* EOF { $1 }
  | toplevel* error { expected "a toplevel statement" $loc($2) }
;

// =========================== toplevel ===========================

toplevel:
  | import { $1 }
  | declaration dot { $1 }
  | definition dot { $1 }
  | type_definition dot { $1 }
  | section { $1 }
  // | global_directive { mknode $sloc (TL_Directive $1) }
;

dot:
  | DOT {}
  | error { expected "a terminating '.'" $sloc }
;

import:
  | HASH import_body { mknode $sloc (TL_Import $2) }
  | HASH error { expected "a path" $loc($2) }
;

declaration:
  LOWERNAME COLON typ {
    let nnode = mknode $loc($1) $1 in
    mknode $sloc (TL_Declaration (nnode, $3))
  }
;

definition:
  binding(expr) { mknode $sloc (TL_Definition $1) }
;

type_definition:
  list(PRIMENAME { mknode $sloc $1}) UPPERNAME COLONEQUAL type_definition_body
    { mknode $sloc (TL_Type_Definition (mktypdecl (mknode $loc($2) $2) $1 $4)) }
;

type_definition_body:
  | variant_type { $1 }
  | typ { $1 }
;

section:
  | LOWERNAME LBRACE toplevel* RBRACE {
    let nnode = mknode $loc($1) $1 in
    mknode $loc (TL_Section (nnode, $3))
  }
  | LOWERNAME LBRACE toplevel* error { unclosed "{" "}" "section" $loc($4) }
;

// ======================== import helpers ========================

import_body: import_body_ { let (d, p, k) = $1 in mkimp d p k }
import_body_:
  | import_rest { let (p, k) = $1 in [], p, k }
  | import_dir_segment import_body_
    { let (d, p, k) = $2 in $1 :: d, p, k } 
;

%inline import_dir_segment:
  | LOWERNAME SLASH { mknode $sloc $1 }
  | UPPERNAME SLASH { mknode $sloc $1 }
;

import_rest:
  // don't question the mess. accept that it (and nothing else) works.
  | snel(DBL_COLON, path_segment) noncolon_import_kind { $1, $2 }
  | list(path_segment DBL_COLON {$1}) colon_import_kind { $1, $2 }
;

%inline path_segment:
  | LOWERNAME { mknode $sloc $1 }
  | UPPERNAME { mknode $sloc $1 }
;

%inline noncolon_import_kind:
  | /* empty */ { mknode $sloc IK_Simple }

  | THICKARROW UPPERNAME { mknode $sloc (IK_Rename $2) }
  | THICKARROW LOWERNAME { mknode $sloc (IK_Rename $2) }
  | THICKARROW error { expected "an identifier" $loc($2) }
;

%inline colon_import_kind:
  | STAR { mknode $sloc IK_Glob }

  | LBRACE snel(COMMA, import_body { mknode $sloc $1 }) RBRACE
    { mknode $sloc (IK_Nested $2) }
  | LBRACE snel(COMMA, import_body) error
    { unclosed "{" "}" "nested imports" $loc($3) }
;

// ======================== binding helpers =======================

t_constr: COLON typ { $2 }

binding(EXPR):
  AT pattern strict_binding(EXPR) {
    let e = match $3 with
      | (Some tnode, e) ->
        {
          span = {e.span with ghost = true};
          item = EX_Constraint (e, tnode);
        }
      | (None, e) -> e
    in
    mkbind $2 e
  }
;

strict_binding(EXPR):
  | pattern strict_binding(EXPR) {
    let (t, e) = $2 in
    t, mkgnode $sloc (EX_Lambda (mkbind $1 e))
  }
  | t_constr? EQUAL EXPR { $1, $3 }
  | error { expected "a pattern or '='" $sloc }
;

lambda_def:
  | ARROW expr { $2 }
  | simple_pattern lambda_def { mkgnode $sloc (EX_Lambda (mkbind $1 $2))}
  | error { expected "a pattern or '->'" $sloc }
;

// =========================== patterns ===========================

pattern: simple_pattern { $1 };

simple_pattern:
  // TODO: rn the pattern's loc seems to be that of
  // its expr instead (when coming from binding)
  | LOWERNAME { mknode $sloc (PA_Variable $1) }
  | UNDERSCORE { mknode $sloc PA_Wildcard }
;

// ========================== expressions ==========================

expr:
  | binding(seq_expr) THICKARROW expr { mknode $sloc (EX_Binding ($1, $3)) }
  | seq_expr { $1 }

  | error { expected "an expression" $sloc }
;

// base expression (sequences and applications)

seq_expr:
  | seq_expr COMMA lambda_expr { mknode $sloc (EX_Sequence ($1, $3)) }
  | lambda_expr { $1 }
;

lambda_expr:
  | BACKSLASH simple_pattern lambda_def { mknode $sloc (EX_Lambda (mkbind $2 $3)) }
  | tuple_expr { $1 }
;

tuple_expr:
  | ssntl(SEMICOLON, constr_expr) { mknode $sloc (EX_Tuple $1) }
  | constr_expr { $1 }
;

constr_expr:
  | constr_expr t_constr { mknode $sloc (EX_Constraint ($1, $2)) }
  | base_expr { $1 }
;

base_expr:
  | upper_longident single_expr?
    { mknode $sloc (EX_Construct ($1, $2)) }
  | single_expr single_expr+
    { mknode $sloc (EX_Application ($1, $2)) }
  | base_expr infix_op base_expr
    { mknode $sloc (EX_Application (mkgnode $loc($2) $2, [$1; $3])) }
  | unary_op base_expr %prec prec_unary
    { mknode $sloc (EX_Application (mkgnode $loc($1) $1, [$2])) }
  | single_expr { $1 }

  | error { expected "an expression" $sloc }
;

%inline infix_op: infix_op_ { mkgop $sloc $1 }
%inline infix_op_:
  | PLUS          { "add" }
  | MINUS         { "sub" }
  | STAR          { "mul" }
  | SLASH         { "div" }
  | CARET         { "pow" }
  | MODULO        { "mod" }
  | DBL_AMPERSAND { "and" }
  | DBL_CARET     { "xor" }
  | DBL_PIPE      { "or"  }
  | EQUAL         { "eq"  }
  | NOTEQUAL      { "neq" }
  | GREATER       { "gr"  }
  | GREATEREQUAL  { "gre" }
  | LESSER        { "ls"  }
  | LESSEREQUAL   { "lse" }
;

%inline unary_op: unary_op_ { mkgop $sloc $1 }
%inline unary_op_:
  | MINUS { "neg" }
  | BANG  { "not" }
;

%inline operator:
  | infix_op { $1 }
  // | unary_op { $1 }
;

single_expr:
  | LPAREN expr RPAREN { grp $2 (mknode $sloc (EX_Grouping $2)) }
  | LPAREN expr error { unclosed "(" ")" "expression" $loc($1) }

  | LBRACKET list_body RBRACKET { mkglist $loc($3) $2 }
  | LBRACKET list_body error { unclosed "[" "]" "list" $loc($1) }

  | INTEGER { mknode $sloc (EX_Literal (LI_Int $1)) }
  | FLOAT { mknode $sloc (EX_Literal (LI_Float $1)) }
  | STRING { mknode $sloc (EX_Literal (LI_String $1)) }
  | CHARACTER { mknode $sloc (EX_Literal (LI_Char $1)) }
  | EMPTYPARENS { mknode $sloc (EX_Literal LI_Nil) }

  | LPAREN operator RPAREN { mknode $sloc $2 }
  | LPAREN operator error { unclosed "(" ")" "operator" $loc($1) }
  | lower_longident { mknode $sloc (EX_Identifier $1) }
  | HASH LOWERNAME { mknode $sloc (EX_External $2) }

  // | directive { mknode $sloc (EX_Directive $1) }
;

%inline list_body:
  separated_list(COMMA, tuple_expr { $sloc, $1 }) { $1 }
;

// ============================= types =============================

// typedef-only

variant_type:
  // rule below would allow variants without leading PIPE but
  // it doesn't work bc it doesn't allow `typ` to take over if this fails
  // | UPPERNAME option(COLON typ { $2 }) PIPE snel(PIPE, variant_constructor)
  //   { mknode $sloc (TY_Variant ((mknode $loc($1) $1, $2) :: $4)) }
  | PIPE snel(PIPE, variant_constructor)
    { mknode $sloc (TY_Variant $2) }
  | PIPE { mknode $sloc (TY_Variant []) }
;

%inline variant_constructor:
  UPPERNAME option(COLON typ { $2 })
    { mknode $loc($1) $1, $2 }
;

// generic

typ:
  | function_type { $1 }
  // | error { expected "a type" $sloc }
;

function_type:
  | function_type ARROW function_type { mknode $sloc (TY_Function ($1, $3)) }
  | tuple_type { $1 }
;

tuple_type:
  | ssntl(SEMICOLON, effect_type) { mknode $sloc (TY_Tuple ($1)) }
  | effect_type { $1 }
;

effect_type:
  | BANG atomic_type { mknode $sloc (TY_Effect (Some $2)) }
  | BANG { mknode $sloc (TY_Effect None) }
  | atomic_type { $1 }
  | error { expected "a type" $sloc }
;

atomic_type:
  | LPAREN typ RPAREN { grp $2 (mknode $sloc (TY_Grouping $2)) }
  | LPAREN typ error { unclosed "(" ")" "type" $loc($1) }

  | BUILTINITY { let (s, w) = $1 in mknode $sloc (TY_Builtin (BT_Int (s, w))) }
  | BUILTINFTY { mknode $sloc (TY_Builtin (BT_Float $1)) }
  | BUILTINVTY { mknode $sloc (TY_Builtin BT_Void) }

  | UNDERSCORE { mknode $sloc TY_Any }
  | PRIMENAME { mknode $sloc (TY_Variable $1) }
  | atomic_type? upper_longident { mknode $sloc (TY_Construct ($1, $2)) }
;

// =========================== directives ===========================

/*
global_directive: DBL_BANG DOT directive_body;

directive: DBL_BANG directive_body;

directive_body:
  | directive_id directive_arg? { mkdir $1 $2 }
  | error { expected "a directive" $sloc }
;

%inline directive_id:
  LOWERNAME {
    (*
    TODO: get name and find out how many args it needs
          and handle that in directive_body?

    PS: id node is `mknode $sloc $1`
    *)
  }
;

%inline directive_arg:
  | BOOL      { mknode $sloc (DA_Bool $1) }
  | INTEGER   { mknode $sloc (DA_Integer $1) }
  | FLOAT     { mknode $sloc (DA_Float $1) }
  | STRING    { mknode $sloc (DA_String $1) }
  | LOWERNAME { mknode $sloc (DA_Identifier $1) }
  | UPPERNAME { mknode $sloc (DA_Identifier $1) }
;
*/

// =========================== longident ===========================

%inline upper_longident: longident_body(UPPERNAME) { mknode $sloc $1 }
%inline lower_longident: longident_body(LOWERNAME) { mknode $sloc $1 }

longident_body(FINAL):
  | longident_body(LOWERNAME) DBL_COLON FINAL { Ident.Cons ($1, $3) } 
  | FINAL { Ident.Ident $1 }
;