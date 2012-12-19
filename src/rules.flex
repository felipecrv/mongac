/* scanner para a linguagem Monga */

%option yylineno

%top{
#include <stdio.h>
#include <stdint.h>
}

%{
extern int yywrap(void) {
    return 1;
}

int mg_i, mg_scan_errno = 0;
char *mg_ch_buffer, *mg_str_buf;
int mg_ch_buffer_next_slot = 0, mg_str_buf_pos = 0;

void mg_debug(const char* msg) {
#ifdef DEBUG
    fputs(msg, stderr);
#endif
}

void mg_dump_token(const char* token_name, const char* token_value) {
    if (token_value) {
        printf("%s(%s) ", token_name, token_value);
    } else {
        printf("%s ", token_name);
    }
}

void mg_error(char *msg) {
    fprintf(stderr, "error:%d: %s\n", yylineno, msg);
    mg_scan_errno = 1;
}

void mg_fatal_error(char *msg) {
    mg_error(msg);
    exit(mg_scan_errno);
}

%}

LNUM       [0-9]+
DNUM       ([0-9]*\.[0-9]+)|([0-9]+\.[0-9]*)
HNUM       0x[0-9a-fA-F]+
ID         [_A-Za-z][_A-Za-z0-9]*
NEWLINE    (\r\n|\n|\r)
WHITE      [ \n\r\t]+

%s MG_COMMENT
%s MG_STRING

%%

<INITIAL>{
"/*"    BEGIN(MG_COMMENT);
"\""    %{
          BEGIN(MG_STRING);
          mg_str_buf = &mg_ch_buffer[mg_ch_buffer_next_slot];
          mg_str_buf_pos = 0;
        %}

if      mg_dump_token("IF", NULL);
else    mg_dump_token("ELSE", NULL);
while          mg_dump_token("WHILE", NULL);
new      mg_dump_token("NEW", NULL);
return      mg_dump_token("RETURN", NULL);
void      mg_dump_token("VOID", NULL);
int       mg_dump_token("INT", NULL);
char      mg_dump_token("CHAR", NULL);
float     mg_dump_token("FLOAT", NULL);
{LNUM}         mg_dump_token("NUMINT", yytext);
{DNUM}         mg_dump_token("NUMFLOAT", yytext);
{ID}      mg_dump_token("ID", yytext);
"("     mg_dump_token("APAR", NULL);
")"     mg_dump_token("FPAR", NULL);
"["       mg_dump_token("ACOL", NULL);
"]"       mg_dump_token("FCOL", NULL);
","       mg_dump_token("VIRG", NULL);
";"       mg_dump_token("PTVIRG", NULL);
"="         mg_dump_token("ATRIB", NULL);
"=="       mg_dump_token("IGUAL", NULL);
"+"      mg_dump_token("SOMA", NULL);
"-"      mg_dump_token("SUB", NULL);
"*"      mg_dump_token("MULT", NULL);
"/"      mg_dump_token("DIV", NULL);
">"      mg_dump_token("MAIORQ", NULL);
"<"      mg_dump_token("MENORQ", NULL);
">="     mg_dump_token("MAIORIG", NULL);
"<="     mg_dump_token("MENORIG", NULL);
"!"      mg_dump_token("NAO", NULL);
"||"     mg_dump_token("OU", NULL);
"&&"     mg_dump_token("E", NULL);
"{"        mg_dump_token("ACHAVE", NULL);
"}"           mg_dump_token("FCHAVE", NULL);
{WHITE}  printf("%s", yytext);
.       mg_error("unexpected char");
}

<MG_COMMENT>{
"*/"      BEGIN(INITIAL);
[^*\n]+   // eat comments in chunks
"*"       // eat the lone star
\n        puts(""); // eat newlines
<<EOF>>   mg_error("unterminated_comment"); yyterminate();
}

<MG_STRING>{
(?s:\\.)   %{
                mg_str_buf[mg_str_buf_pos++] = yytext[0];
                mg_str_buf[mg_str_buf_pos++] = yytext[1];
            %}
[^\\\"]+    %{
                memcpy(&mg_str_buf[mg_str_buf_pos], yytext, strlen(yytext));
                mg_str_buf_pos += strlen(yytext);
            %}
{NEWLINE}   mg_error("missing \""); BEGIN(INITIAL);
\"          %{
                mg_dump_token("STRING", mg_str_buf);
                mg_ch_buffer_next_slot += mg_str_buf_pos;
                BEGIN(INITIAL);
            %}
}

%%

int main(int argc, char *argv[]) {
    mg_ch_buffer = (char *) yyalloc(YY_BUF_SIZE);
    memset(mg_ch_buffer, 0, YY_BUF_SIZE);
    yylex();
    yyfree(mg_ch_buffer);
    return mg_scan_errno;
}
