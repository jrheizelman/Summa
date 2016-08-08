%{
    #include "node.h"
    Program *program; /* the top level root node of our final AST */

    extern int yylex();
    void yyerror(const char *s) { printf("ERROR: %sn", s); }
%}

/* Represents the many different ways we can access our data */
%union {
    Rval *rval;
    Stmt *stmt;
    Block *block;
    std::string *string;
    int token;
    Lval *lval;
    RvalList *actuals;
    Var_defList *params;
    Var_def *var_def;
    Type *type;
    Program *program;
    Func_def *func_def;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> ID INT_LIT DOUB_LIT BOOL_LIT CHAR_LIT STRING_LIT
%token SEMI COMMA IF WHILE FOR RETURN DOT BOOL INT STRING CHAR DOUBLE ASSIGN
%token CONTINUE FUNC VOID

%type <stmt> stmt
%type <rval> rval
%type <lval> lval
%type <block> block stmt_list
%type <actuals> actuals_opt actuals_list
%type <params> params_opt params_list
%type <var_def> var_def
%type <type> type
%type <func_def> func_def
%type <program> program

/* Operator precedence for mathematical operators */
%nonassoc NOELSE
%nonassoc ELSE
%left DOT
%left <token> OR AND
%left <token> EQ NEQ LEQ GEQ LTHAN GTHAN
%left <token> PLUS MINUS
%left <token> MULT DIV MOD
%right <token> NOT
%right NEG
%left LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK

%start program

%%

program :
	func_def { $$ = new Program(); $$->functions.push_back($1);  }
	| program func_def { $1->functions.push_back($2); }
	;

func_def :
	FUNC ID LPAREN params_opt RPAREN block {
		$$ = new Func_def(*new Id(*$2), *$4, *$6); }
	;

params_opt :
	/* nothing */ { $$ = new Var_defList(); }
	| params_list { $$ = $1; }
	;

params_list :
	var_def { $$ = new Var_defList(); $$->push_back($1); }
	| params_list COMMA var_def { $1->push_back($3); }
	;

var_def :
	type ID { $$ = new Var_def(*$1, *new Id(*$2)); }
	;

type :
	/* Language-defined types */
	INT { $$ = new Type("int"); }
	| DOUBLE { $$ = new Type("double"); }
	| CHAR { $$ = new Type("char"); }
	| STRING { $$ = new Type("string"); }
	| BOOL { $$ = new Type("bool"); }
	| VOID { $$ = new Type("void"); }
	/* User-defined types */
	| ID { $$ = new Type(*$1); }
	/* Array type declaration */
	| type LBRACK RBRACK { $1->arr_dim++; }
	;

block :
	LBRACE stmt_list RBRACE { $$ = $2; }
	| LBRACE RBRACE { $$ = new Block(); }
	;

stmt_list :
	stmt { $$ = new Block(); $$->stmt_list.push_back($1); }
	| stmt_list stmt { $1->stmt_list.push_back($2); }
	;

stmt :
	lval ASSIGN rval SEMI { $$ = new Assign(*$1, *$3); }
	| RETURN rval SEMI { $$ = new Return(*$2); }
	| rval SEMI { }
	| IF LPAREN rval RPAREN block %prec NOELSE {
		$$ = new If(*$3, *$5, *new Block()); }
	| IF LPAREN rval RPAREN block ELSE block {
		$$ = new If(*$3, *$5, *$7); }
	| FOR LPAREN rval RPAREN block {
		$$ = new For(*new Stmt(), *$3, *new Stmt(), *$5); }
	| FOR LPAREN stmt SEMI rval SEMI stmt RPAREN block {
		$$ = new For(*$3, *$5, *$7, *$9); }
	;

lval :
	ID { $$ = new Id(*$1); }
	| lval DOT lval { $$ = new Access(*$1, *$3); }
	| lval LBRACK rval RBRACK { $$ = new Access_arr(*$1, *$3); }
	;

rval :
	/* Literals */
	INT_LIT { $$ = new Int_lit(atol($1->c_str())); delete $1; }
	| DOUB_LIT { $$ = new Doub_lit(atof($1->c_str())); delete $1; }
	| CHAR_LIT { $$ = new Char_lit($1->c_str()[1]); delete $1; }
	| STRING_LIT { $$ = new Str_lit($1->substr(0, $1->length()-2)); delete $1; }
	| BOOL_LIT { $$ = new Bool_lit($1->compare("true")); delete $1; }
	/* Accesses and function calls */
	| lval { $$ = new Access_lval(*$1); }
	| lval LPAREN actuals_opt RPAREN { $$ = new Func_call(*$1, *$3); }
	/* Unary operations */
	| MINUS rval %prec NEG { $$ = new UnaryOperator($1, *$2); }
	| NOT rval { $$ = new UnaryOperator($1, *$2); }
	/* Binary operations */
	| rval PLUS rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval MULT rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval MINUS rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval DIV rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval MOD rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval EQ rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval NEQ rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval LEQ rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval GEQ rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval LTHAN rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval GTHAN rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval OR rval { $$ = new BinaryOperator(*$1, $2, *$3); }
	| rval AND rval { $$ = new BinaryOperator(*$1, $2, *$3); }
    ;

actuals_opt:
	/* nothing */ { $$ = new RvalList(); }
	| actuals_list { $$ = $1; }
	;

actuals_list:
	rval { $$ = new RvalList(); $$->push_back($1); }
	| actuals_list COMMA rval { $1->push_back($3); }
	;

%%
