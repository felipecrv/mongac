#include <iostream>
#include <cstring>
#include "tokens.h"
#include "ast.h"
#include "exception.h"
using namespace monga;

extern Prog* program;
extern int yyparse();
extern int semantic_check_success;

int main(int argc, char* argv[]) {
    mg_scanner_init();
    yyparse();
    int scanner_err = mg_scanner_finish();
    if (scanner_err) {
        return scanner_err;
    }

    if (argc > 1 && strcmp(argv[1], "--dump-ast") == 0) {
        std::cout << program->toStr() << std::endl;
    }

    // type checks the program
    Env env;
    try {
        program->typeCheck(&env, the_void_type());
    } catch (SemanticExn& e) {
        e.emitError();
    }
    return !semantic_check_success;
}
