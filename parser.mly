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

%token LPAREN RPAREN
%token PLUS TIMES MINUS DIVIDE MOD
%token AND OR NOT EQ NEQ LEQ GEQ LTHAN GTHAN
%token SEMI ASSIGN EOF
%token <int> INTLIT
%token <bool> BOOLLIT
%token <float> DOUBLELIT
%token <string> ID

/* state precedence of tokens - need this to avoid shift/reduce conflicts */
/* goes from least to most important in precedence */
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

lval:
  ID  { Id($1) }

stmt:
  lval ASSIGN rval SEMI   { Assign($1, $3) }
| rval SEMI   { Rval($1) }

stmt_list:
  /* nothing */   { [] }
| stmt_list stmt { $2 :: $1 }

program:
  stmt_list EOF   { $1 }