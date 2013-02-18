#include <iostream>
#include "tokens.h"
#include "ast.h"
using namespace monga;

extern MongaProg* program;

extern int yyparse();

int main() {
    yyparse();

    return 0;
}
