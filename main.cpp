#include <iostream>
#include "tokens.hpp"
// extern int yyparse(void *scanner);
// extern int yylex_init (void *scanner);

int main(int argc, char **argv)
{
	// yylloc.first_line = yylloc.last_line = 1;
	// yylloc.first_column = yylloc.last_column = 0;
    // yyparse(NULL);
	yyscan_t scanner;
	if (yylex_init(&scanner)) exit(1);
	if (yyparse(scanner)) exit(1);
	yylex_destroy(scanner);
    // std::cout << programBlock << std::endl;
    return 0;
}