#include <iostream>
#include <cstring>
#include "tokens.h"
#include "ast.h"
#include "exception.h"
#include "PascalCodeGen.h"
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

    // type checks the program
    semantic_check_success = 1;
    Env env;
    try {
        program->typeCheck(&env, the_void_type());
    } catch (SemanticExn& e) {
        e.emitError();
    }

    if (!semantic_check_success) {
        return 1;
    }

    PascalCodeGen codegen;
    codegen.gen(program, cout);
    return 0;
}
