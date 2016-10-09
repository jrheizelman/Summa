%{ open Ast

let scope = ref 1 (*contents of scope == 1*)

let inc_block_num
 (u:unit) =
    let x = scope.contents in
    scope := x + 1; x (*set the contents of scope to x+1, increments it by 1*)


let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout

%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI ASSIGN
%token RETURN INT CHAR STRING DOUBLE COMMA
%token PLUS TIMES MINUS DIVIDE MOD NOT EQ NEQ LEQ GEQ LTHAN GTHAN
%token AND OR IF ELSE WHILE FOR RETURN CONTINUE BREAK
%token BOOL DOT EOF DEF CLASS
%token <int> INTLIT
%token <bool> BOOLLIT
%token <float> DOUBLIT
%token <string> ID STRINGLIT CHARLIT

/* state precedence of tokens - need this to avoid shift/reduce conflicts */
/* goes from least to most important in precedence */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LEQ GEQ LTHAN GTHAN
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right NEG
%left LPAREN RPAREN LBRACK RBRACK

%start program
%type <Ast.program> program
%%

stmt:
  lval ASSIGN rval SEMI   { Assign($1, $3) }
| RETURN rval SEMI    { Return($2) }
| rval SEMI   { Expr($1) }
| IF LPAREN rval RPAREN block %prec NOELSE
  { If($3, $5, { statements = []; block_num = scope.contents } ) }
| IF LPAREN rval RPAREN block ELSE block    { If ($3, $5, $7) }
| WHILE LPAREN rval RPAREN block    { While($3, $5) }

lval:
  ID    { Id($1) }
/*| ID DOT lval   { Access($1, $3) }*/
/*| ID LBRACK rval RBRACK                     { Access_arr($1, $3) }*/

rval:
  lval    { Access_lval($1) }
| CHARLIT   { Char_lit($1) }
| INTLIT    { Int_lit($1) }
| STRINGLIT   { String_lit($1) }
| BOOLLIT   { Bool_lit($1) }
| lval LPAREN actuals_opt RPAREN   { Func_call($1, $3) }
/* Unary operations */
| NOT rval    { Un_op(Not, $2) }
| MINUS rval %prec NEG    { Un_op(Neg, $2) }
/* Binary operations */
| rval PLUS rval    { Bin_op($1, Add, $3) }
| rval MINUS rval    { Bin_op($1, Sub, $3) }
| rval TIMES rval    { Bin_op($1, Mult, $3) }
| rval DIVIDE rval    { Bin_op($1, Div, $3) }
| rval MOD rval    { Bin_op($1, Mod, $3) }
| rval EQ rval   { Bin_op($1, Equal, $3) }
| rval NEQ rval   { Bin_op($1, Neq, $3) }
| rval LEQ rval   { Bin_op($1, Leq, $3) }
| rval GEQ rval   { Bin_op($1, Geq, $3) }
| rval LTHAN rval   { Bin_op($1, Less, $3) }
| rval GTHAN rval   { Bin_op($1, Greater, $3) }
| rval OR rval   { Bin_op($1, Or, $3) }
| rval AND rval   { Bin_op($1, And, $3) }

actuals_opt:
  /* nothing */  { [] }
| actuals_list { List.rev $1 }

actuals_list:
  rval                      { [$1] }
| actuals_list COMMA rval { $3 :: $1 }

func_def:
  DEF ID LPAREN params_opt RPAREN block   { { fname = $2;
                                              params = $4;
                                              fblock = $6; } }

block:
  LBRACE stmt_list RBRACE   { { statements = List.rev $2;
                                block_num = inc_block_num() } }

stmt_list:
  /* nothing */   { [] }
| stmt_list stmt  { $2 :: $1 }

params_opt:
  /* nothing */   { [] }
| params_list   { List.rev $1 }

params_list:
  param_def   { [$1] }
| params_list COMMA param_def   { $3 :: $1 }

param_def:
  type_decl ID    { ($1, $2) }

type_decl:
  INT     { Int }
| CHAR    { Char }
| STRING    { String }
| BOOL    { Bool }
| DOUBLE    { Double }

glb_vdecl:
  lval ASSIGN rval SEMI   { ($1, $3) }

program:
  /* nothing */   { [], [] }
| program glb_vdecl { ($2 :: fst $1), snd $1 }
| program func_def { fst $1, ($2 :: snd $1) }