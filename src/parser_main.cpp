#include <iostream>
#include <cstring>
#include "tokens.h"
#include "ast.h"
#include "exception.h"
using namespace monga;

extern Prog* program;

extern int yyparse();

int main(int argc, char* argv[]) {
    mg_scanner_init();
    yyparse();
    if (argc > 1 && strcmp(argv[1], "--dump-ast") == 0) {
        std::cout << program->toStr() << std::endl;
    }

    // type checks the program
    Env env;
    try {
        program->typeCheck(&env);
    } catch (SymbolNotFoundExn& e) {
        std::cerr << "error: " << e.what() << std::endl;
    } catch (InvalidAssignExn& e) {
        std::cerr << "error: " << e.what() << std::endl;
    }

    return mg_scanner_finish();
}
