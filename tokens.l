%{
#include <string>
#include "node.h"
#include "parser.hpp"
#define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t) (yylval.token = t)
extern "C" int yywrap() { return 1; }
%}

%%

[ tn]               	;
"/\*[\n|.]*\*/"
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
"true"|"false"			SAVE_TOKEN; return BOOL_LIT;
"\"[^\"]*\""			SAVE_TOKEN; return STRING_LIT;
"\'[^']?\'"				SAVE_TOKEN; return CHAR_LIT;
"[0-9]+.[0-9]*"			SAVE_TOKEN; return DOUB_LIT;
"[0-9]+"        		SAVE_TOKEN; return INT_LIT;
"[a-z][a-z A-Z 0-9 _]*"	SAVE_TOKEN; return ID;
"."						return TOKEN(DOT);
.              			printf("Unknown token!n"); yyterminate();

%%