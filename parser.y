%code requires{
	#include "node.h"

    /* An opaque pointer. */
	#ifndef YY_TYPEDEF_YY_SCANNER_T
	#define YY_TYPEDEF_YY_SCANNER_T
	typedef void* yyscan_t;
	#endif
}

%error-verbose
%locations
%define api.pure
%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner}

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
    Glob_var *glob_var;
    Class_def *class_def;
}

%{
	extern int yylex(YYSTYPE *lvalp, YYLTYPE *llocp, void *yyscanner);
	void yyerror(YYLTYPE *llocp, void *yyscanner, const char *s) {
		printf("Error: %d.%d-%d.%d: %s\n", llocp->first_line,
			llocp->first_column, llocp->last_line, llocp->last_column, s);
		std::exit(1);
	}
%}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> ID INT_LIT DOUB_LIT BOOL_LIT CHAR_LIT STRING_LIT CLASS_ID
%token SEMI COMMA IF WHILE FOR RETURN DOT BOOL INT STRING CHAR DOUBLE ASSIGN
%token CONTINUE FUNC VOID CLASS

%type <stmt> stmt
%type <rval> rval
%type <lval> lval
%type <block> block stmt_list
%type <actuals> actuals_opt actuals_list
%type <params> params_opt params_list
%type <var_def> var_def
%type <type> type type_def
%type <func_def> func_def
%type <glob_var> glob_var
%type <class_def> class_def class_body
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
	| program glob_var { $1->global_vars.push_back($2); }
	| program class_def { $1->classes.push_back($2); }
	;

glob_var :
	ID ASSIGN rval SEMI { $$ = new Glob_var(*new Id(*$1), *$3); }
	;

class_def :
	CLASS ID LPAREN params_opt RPAREN LBRACE class_body RBRACE {
		$$ = $7; $$->id = *$2; $$->class_vars = *$4; }
	;

class_body :
	%empty { $$ = new Class_def(); }
	| class_body func_def { $1->class_funcs.push_back($2); }
	;

func_def :
	FUNC ID LPAREN params_opt RPAREN block {
		$$ = new Func_def(*new Id(*$2), *$4, *$6); }
	;

params_opt :
	%empty { $$ = new Var_defList(); }
	| params_list { $$ = $1; }
	;

params_list :
	var_def { $$ = new Var_defList(); $$->push_back($1); }
	| params_list COMMA var_def { $1->push_back($3); }
	;

var_def :
	type_def ID { $$ = new Var_def(*$1, *new Id(*$2)); }
	;

type_def : /* Needed to allow for array declarations */
	type { $$ = $1; }
	| type LBRACK RBRACK { $1->arr_dim++; }
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
	| CLASS_ID { $$ = new Type(*$1); }
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
	| rval SEMI { $$ = new Stmt(); }
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
	%empty { $$ = new RvalList(); }
	| actuals_list { $$ = $1; }
	;

actuals_list:
	rval { $$ = new RvalList(); $$->push_back($1); }
	| actuals_list COMMA rval { $1->push_back($3); }
	;

%%
