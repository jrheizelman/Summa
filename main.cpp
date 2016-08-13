#include <iostream>
#include "tokens.hpp"
// extern int yyparse(void *scanner);
// extern int yylex_init (void *scanner);

int main(int argc, char **argv)
{
	yyscan_t scanner;
	yylex_init(&scanner);
    yyparse(scanner);
    printf("Parsing successful\n");
    yylex_destroy(scanner);
    // std::cout << programBlock << std::endl;
    return 1;
}