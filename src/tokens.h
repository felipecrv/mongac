#ifndef MONGA_TOKENS_
#define MONGA_TOKENS_

void mg_scanner_init();
int mg_scanner_finish();
extern "C" int yylex();
extern  "C" int yylineno;

#endif // MONGA_TOKENS_
