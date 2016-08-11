%{
#include <string>
#define SAVE_TOKEN yylval->string = new std::string(yytext, yyleng)
#define TOKEN(t) (yylval->token = t)
#define YY_USER_ACTION  yylloc->last_column += yyleng;
%}

%top{
#include "parser.hpp"

/* Initialize LOC. */
# define LOCATION_RESET(Loc)                  \
	(Loc).first_column = (Loc).first_line = 1;  \
	(Loc).last_column =  (Loc).last_line = 1;

/* Advance of NUM lines. */
# define LOCATION_LINES(Loc, Num)             \
	(Loc).last_column = 1;                      \
	(Loc).last_line += Num;

/* Restart: move the first cursor to the last position. */
# define LOCATION_STEP(Loc)                   \
	(Loc).first_column = (Loc).last_column;     \
	(Loc).first_line = (Loc).last_line;

/* Output LOC on the stream OUT. */
# define LOCATION_PRINT(Out, Loc)							\
	if ((Loc).first_line != (Loc).last_line)				\
		fprintf (Out, "%d.%d-%d.%d",						\
			(Loc).first_line, (Loc).first_column,			\
			(Loc).last_line, (Loc).last_column - 1);		\
	else if ((Loc).first_column < (Loc).last_column - 1)	\
		fprintf (Out, "%d.%d-%d", (Loc).first_line,			\
			(Loc).first_column, (Loc).last_column - 1);		\
	else													\
		fprintf (Out, "%d.%d", (Loc).first_line, (Loc).first_column)
}

%option reentrant
%option bison-bridge
%option bison-locations
%option outfile="tokens.cpp" header-file="tokens.hpp"
%option yylineno
%option noyywrap

%%

%{
	/* At each yylex invocation, mark the current position as the
		start of the next token.  */
	LOCATION_STEP (*yylloc);
%}

[\t ]               	LOCATION_STEP(*yylloc);
"/\*[\n|.]*\*/"			LOCATION_STEP(*yylloc);
\n+						LOCATION_LINES(*yylloc, yyleng); LOCATION_STEP(*yylloc);
"("             		return TOKEN(LPAREN);
")"             		return TOKEN(RPAREN);
"{"						return TOKEN(LBRACE);
"}"						return TOKEN(RBRACE);
"["						return TOKEN(RBRACK);
"]"						return TOKEN(RBRACK);
";"						return TOKEN(SEMI);
","						return TOKEN(COMMA);
"="						return TOKEN(ASSIGN);
"=="					return TOKEN(EQ);
"!="					return TOKEN(NEQ);
"<="					return TOKEN(LEQ);
">="					return TOKEN(GEQ);
"<"						return TOKEN(LTHAN);
">"						return TOKEN(GTHAN);
"!"						return TOKEN(NOT);
"&&"					return TOKEN(AND);
"||"					return TOKEN(OR);
"if"					return TOKEN(IF);
"else"					return TOKEN(ELSE);
"while"					return TOKEN(WHILE);
"for"					return TOKEN(FOR);
"continue"				return TOKEN(CONTINUE);
"return"				return TOKEN(RETURN);
"func"					return TOKEN(FUNC);
"class"					return TOKEN(CLASS);
"+"             		return TOKEN(PLUS);
"-"             		return TOKEN(MINUS);
"*"             		return TOKEN(MULT);
"/"             		return TOKEN(DIV);
"%"						return TOKEN(MOD);
"bool"					return TOKEN(BOOL);
"string"				return TOKEN(STRING);
"char"					return TOKEN(CHAR);
"int"					return TOKEN(INT);
"double"				return TOKEN(DOUBLE);
"void"					return TOKEN(VOID);
"true"|"false"			SAVE_TOKEN; return BOOL_LIT;
"\"[^\"]*\""			SAVE_TOKEN; return STRING_LIT;
"\'[^']?\'"				SAVE_TOKEN; return CHAR_LIT;
[0-9]+.[0-9]*			SAVE_TOKEN; return DOUB_LIT;
[0-9]+        			SAVE_TOKEN; return INT_LIT;
[a-z][a-z A-Z 0-9 _]*	SAVE_TOKEN; return ID;
[A-Z][a-z A-Z 0-9 _]* 	SAVE_TOKEN; return CLASS_ID;
"."						return TOKEN(DOT);
.              			{
							printf("Error: %d.%d-%d.%d: Unknown token\n",
								yylloc->first_line, yylloc->first_column,
								yylloc->last_line, yylloc->last_column);
							std::exit(1);
						}

%%