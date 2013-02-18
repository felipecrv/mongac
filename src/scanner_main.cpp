#include "tokens.h"

int main(int argc, char *argv[]) {
    mg_scanner_init();
    yylex();
    return mg_scanner_finish();
}
