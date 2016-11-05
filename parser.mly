%{ open Ast

let scope = ref 1 (*contents of scope == 1*)

let inc_block_num
 (u:unit) =
    let x = scope.contents in
    scope := x + 1; x (*set the contents of scope to x+1, increments it by 1*)

%}

%token LPAREN RPAREN LBRACE RBRACE
%token WHILE IF ELSE RETURN NOELSE FOR DEF
%token PLUS TIMES MINUS DIVIDE MOD PLUSPLUS MINUSMINUS
%token BOOL INT DOUBLE VOID
%token AND OR NOT EQ NEQ LEQ GEQ LTHAN GTHAN
%token SEMI ASSIGN EOF COMMA
%token <int> INTLIT
%token <bool> BOOLLIT
%token <float> DOUBLELIT
%token <string> ID

/* state precedence of tokens - need this to avoid shift/reduce conflicts */
/* goes from least to most important in precedence */
%nonassoc NOELSE
%nonassoc ELSE
%left AND OR
%left EQ NEQ
%left LEQ GEQ LTHAN GTHAN
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NEG

%start program
%type <Ast.program> program
%%

rval:
  INTLIT    { Int_lit($1) }
| BOOLLIT   { Bool_lit($1) }
| DOUBLELIT   { Double_lit($1) }
/* Unary operations */
| MINUS rval %prec NEG    { Un_op(Neg, $2) }
| NOT rval %prec NEG    { Un_op(Not, $2) }
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
| LPAREN rval RPAREN  { $2 }
| lval  { Access_lval($1) }
| PLUSPLUS lval  { Increment(Incr_front, $2) }
| lval PLUSPLUS  { Increment(Incr_back, $1) }
| MINUSMINUS lval  { Increment(Decr_front, $2) }
| lval MINUSMINUS  { Increment(Decr_back, $1) }

opt_rval:
  /* nothing */ { Noexpr }
| rval  { $1 }

valid_type:
  INT   { Mono(Int, [Num]) }
| DOUBLE  { Mono(Double, [Num]) }
| BOOL  { Mono(Bool, [None]) }
| VOID { Mono(Void, [None]) }

lval:
  ID  { Id($1) }

line_stmt:
  lval ASSIGN rval   { Assign($1, $3) }
| rval   { Rval($1) }
| RETURN opt_rval   { Return($2) }

stmt:
  line_stmt SEMI { $1 }
| IF LPAREN rval RPAREN block %prec NOELSE {
    If($3, $5, { block_num = scope.contents; stmts = [] }) }
| IF LPAREN rval RPAREN stmt %prec NOELSE   {
    If($3, { block_num = scope.contents; stmts = [$5] }, { block_num = scope.contents; stmts = [] }) }
| IF LPAREN rval RPAREN block ELSE block  { If($3, $5, $7) }
| IF LPAREN rval RPAREN stmt ELSE block   {
    If($3, { block_num = scope.contents; stmts = [$5] }, $7) }
| IF LPAREN rval RPAREN block ELSE stmt   {
    If($3, $5, { block_num = scope.contents; stmts = [$7] }) }
| IF LPAREN rval RPAREN stmt ELSE stmt   {
    If($3, { block_num = scope.contents; stmts = [$5] }, { block_num = scope.contents; stmts = [$7] }) }
| WHILE LPAREN opt_rval RPAREN block  {
    match $3 with
      Noexpr -> While(Bool_lit(true), $5)
    | _ -> While($3, $5) }
| WHILE LPAREN opt_rval RPAREN stmt  {
    match $3 with
      Noexpr -> While(Bool_lit(true), { block_num = scope.contents; stmts = [$5] } )
    | _ -> While($3, { block_num = scope.contents; stmts = [$5] } ) }
| FOR LPAREN line_stmt SEMI rval SEMI line_stmt RPAREN block  { For($3, $5, $7, $9) }
| FOR LPAREN line_stmt SEMI rval SEMI line_stmt RPAREN stmt  {
    For($3, $5, $7, { block_num = scope.contents; stmts = [$9] }) }

stmt_list:
  /* nothing */   { [] }
| stmt_list stmt { $2 :: $1 }

block:
  LBRACE stmt_list RBRACE   { { stmts = List.rev $2; block_num = inc_block_num() } }

param:
  ID  { (Poly(Conditioned([])), $1) }
| valid_type ID { ($1, $2) }

params_opt:
  /* nothing */   { [] }
| param   { [$1] }
| params_opt COMMA param  { $3 :: $1 }

func_def:
  DEF ID LPAREN params_opt RPAREN block   {
    { id = $2; ret_type = Poly(Conditioned([])); params = List.rev $4; body_block = $6 } }
| valid_type ID LPAREN params_opt RPAREN block   {
    { id = $2; ret_type = $1; params = List.rev $4; body_block = $6 } }

program:
  func_def EOF   { $1 }
