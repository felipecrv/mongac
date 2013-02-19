#include <iostream>
#include "tokens.h"
#include "ast.h"
using namespace monga;

extern MongaProg* program;

extern int yyparse();

int main() {
    mg_scanner_init();
    yyparse();
    return mg_scanner_finish();
}
