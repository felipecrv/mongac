/* scanner para a linguagem Monga */

%option yylineno

%top{
#include <cstdio>
#include <cstdarg>
#include "tokens.h"
}

%{
static int mg_scan_errno = 0; // error code
static char *mg_ch_buffer, *mg_str_buf;
static yy_size_t mg_ch_buffer_next_slot = 0, mg_str_buf_pos = 0;
static char mg_unexpected_char[3];

static void mg_error(const char*, ...);
static void mg_dump_token(const char*, const char*);
static void mg_debug(const char*, ...);
static void mg_char_to_printable(char, char*);
%}

LNUM       [0-9]+
DNUM       ([0-9]*\.[0-9]+)|([0-9]+\.[0-9]*)
HNUM       0[xX][0-9a-fA-F]+
ID         [_A-Za-z][_A-Za-z0-9]*
NEWLINE    (\r\n|\n|\r)
WHITE      [ \n\r\t]+

%s MG_COMMENT
%s MG_STRING
%s MG_CHAR

%%

<INITIAL>{
"/*"    BEGIN(MG_COMMENT); mg_debug("BEGIN(MG_COMMENT)\n");
"\""    %{
           BEGIN(MG_STRING);
           mg_debug("BEGIN(MG_STRING)\n");
           mg_str_buf = &mg_ch_buffer[mg_ch_buffer_next_slot];
           mg_str_buf_pos = 0;
        %}
"'"     %{
          BEGIN(MG_CHAR);
          mg_debug("BEGIN(MG_CHAR)\n");
          mg_str_buf = &mg_ch_buffer[mg_ch_buffer_next_slot];
          mg_str_buf_pos = 0;
        %}
if      mg_dump_token("IF", NULL);
else    mg_dump_token("ELSE", NULL);
while   mg_dump_token("WHILE", NULL);
new     mg_dump_token("NEW", NULL);
return  mg_dump_token("RETURN", NULL);
void    mg_dump_token("VOID", NULL);
int     mg_dump_token("INT", NULL);
char    mg_dump_token("CHAR", NULL);
float   mg_dump_token("FLOAT", NULL);
{HNUM}  mg_dump_token("NUMINT", yytext);
{LNUM}  mg_dump_token("NUMINT", yytext);
{DNUM}  mg_dump_token("NUMFLOAT", yytext);
{ID}    mg_dump_token("ID", yytext);
"("     mg_dump_token("APAR", NULL);
")"     mg_dump_token("FPAR", NULL);
"["     mg_dump_token("ACOL", NULL);
"]"     mg_dump_token("FCOL", NULL);
","     mg_dump_token("VIRG", NULL);
";"     mg_dump_token("PTVIRG", NULL);
"="     mg_dump_token("ATRIB", NULL);
"=="    mg_dump_token("IGUAL", NULL);
"+"     mg_dump_token("SOMA", NULL);
"-"     mg_dump_token("SUB", NULL);
"*"     mg_dump_token("MULT", NULL);
"/"     mg_dump_token("DIV", NULL);
">"     mg_dump_token("MAIORQ", NULL);
"<"     mg_dump_token("MENORQ", NULL);
">="    mg_dump_token("MAIORIG", NULL);
"<="    mg_dump_token("MENORIG", NULL);
"!"     mg_dump_token("NAO", NULL);
"||"    mg_dump_token("OU", NULL);
"&&"    mg_dump_token("E", NULL);
"{"     mg_dump_token("ACHAVE", NULL);
"}"     mg_dump_token("FCHAVE", NULL);
{WHITE} printf("%s", yytext); // output the spaces
.       %{
            mg_char_to_printable(yytext[0], (char *) mg_unexpected_char);
            mg_error("unexpected character '%s'", mg_unexpected_char);
        %}
}

<MG_COMMENT>{
"*/"      BEGIN(INITIAL);
[^*\n]+   // eat comments in chunks
"*"       // eat the lone star
\n        putchar('\n'); // eat newlines and output them
<<EOF>>   mg_error("unterminated_comment"); yyterminate();
}

<MG_STRING>{
(?s:\\.)      %{
                  switch (yytext[1]) {
                  case 'n':
                      mg_str_buf[mg_str_buf_pos++] = '\n';
                      break;
                  case 't':
                      mg_str_buf[mg_str_buf_pos++] = '\t';
                      break;
                  case 'r':
                      mg_str_buf[mg_str_buf_pos++] = '\r';
                      break;
                  case '"':
                      mg_str_buf[mg_str_buf_pos++] = '"';
                      break;
                  case '0':
                      mg_str_buf[mg_str_buf_pos++] = '\0';
                      break;
                  case '\\':
                      mg_str_buf[mg_str_buf_pos++] = '\\';
                      break;
                  case '\n':
                      // ignore the newline after backslash
                      break;
                  default:
                      mg_char_to_printable(yytext[1], mg_unexpected_char);
                      mg_error(
                          "invalid escape sequence. unexpected '%s' after \\",
                          mg_unexpected_char);
                      mg_str_buf[mg_str_buf_pos++] = '\\';
                      break;
                  }
              %}
[^\\\"\n\r]+  %{
                  memcpy(&mg_str_buf[mg_str_buf_pos], yytext, strlen(yytext));
                  mg_str_buf_pos += strlen(yytext);
              %}
{NEWLINE}     mg_error("missing terminating character \""); BEGIN(INITIAL);
\"            %{
                  mg_dump_token("STRING", mg_str_buf);
                  mg_ch_buffer_next_slot += mg_str_buf_pos;
                  BEGIN(INITIAL);
              %}
}

<MG_CHAR>{
(?s:\\.)      %{
                  switch (yytext[1]) {
                  case 'n':
                      mg_str_buf[mg_str_buf_pos++] = '\n';
                      break;
                  case 't':
                      mg_str_buf[mg_str_buf_pos++] = '\t';
                      break;
                  case 'r':
                      mg_str_buf[mg_str_buf_pos++] = '\r';
                      break;
                  case '\'':
                      mg_str_buf[mg_str_buf_pos++] = '\'';
                      break;
                  case '0':
                      mg_str_buf[mg_str_buf_pos++] = '\0';
                      break;
                  case '\\':
                      mg_str_buf[mg_str_buf_pos++] = '\\';
                      break;
                  case '\n':
                      // ignore the newline after backslash
                      break;
                  default:
                      mg_char_to_printable(yytext[1], mg_unexpected_char);
                      mg_error(
                          "invalid escape sequence. unexpected '%s' after \\",
                          mg_unexpected_char);
                      mg_str_buf[mg_str_buf_pos++] = '\\';
                      break;
                  }
              %}
[^\\\'\n\r]+  %{
                  memcpy(&mg_str_buf[mg_str_buf_pos], yytext, strlen(yytext));
                  mg_str_buf_pos += strlen(yytext);
              %}
{NEWLINE}     mg_error("missing terminating character \""); BEGIN(INITIAL);
"'"           %{
                  mg_dump_token("STRING", mg_str_buf);
                  mg_ch_buffer_next_slot += mg_str_buf_pos;
                  BEGIN(INITIAL);
              %}
}

%%

int yywrap(void) {
    return 1;
}

void mg_scanner_init() {
    mg_ch_buffer = (char *) yyalloc(YY_BUF_SIZE);
    memset(mg_ch_buffer, 0, YY_BUF_SIZE);
}

int mg_scanner_finish() {
    yyfree(mg_ch_buffer);
    return mg_scan_errno;
}

static void mg_error(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    mg_scan_errno = 1;
    fprintf(stderr, "error:%d: ", yylineno);
    vfprintf(stderr, fmt, ap);
    putchar('\n');
    va_end(ap);
}

static void mg_dump_token(const char* token_name, const char* token_value) {
    if (token_value) {
        printf("%s(%s) ", token_name, token_value);
    } else {
        printf("%s ", token_name);
    }
}

static void mg_debug(const char* fmt, ...) {
#ifdef DEBUG
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
#endif
}

static void mg_char_to_printable(char c, char* print) {
    switch (c) {
    case '\n':
        print[0] = '\\';
        print[1] = 'n';
        print[2] = '\0';
        break;
    case '\t':
        print[0] = '\\';
        print[1] = 't';
        print[2] = '\0';
        break;
    case '\r':
        print[0] = '\\';
        print[1] = 'r';
        print[2] = '\0';
        break;
    default:
        print[0] = c;
        print[1] = '\0';
        break;
    }
}
