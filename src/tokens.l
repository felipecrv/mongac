/* scanner para a linguagem Monga */

%option yylineno

%top{
#include <cstdio>
#include <cstdarg>
#include <string>
#include <vector>
#include "tokens.h"
#include "ast.h"
using namespace monga; // necessary inside the bison-generated parser.h
#include "parser.h"

#ifdef MG_SCANNER
  // Dump tokens if MG_SCANNER is defined
  #define TOK(t)         mg_dump_token(#t, NULL)
  #define VAL_TOK(t)     mg_dump_token(#t, yytext)
  #define STR_TOK(t, st) mg_dump_token(#t, st)
#else
  // Return the tokens if it's not just a scanner
  #ifdef DEBUG
    #define TOK(t)         mg_dump_token(#t, NULL); return (yylval.token = t)
    #define VAL_TOK(t)     mg_dump_token(#t, yytext); yylval.string = new std::string(yytext, yyleng); return (t)
    #define STR_TOK(t, st) mg_dump_token(#t, st); yylval.string = new std::string(st); return (t)
  #else
    #define TOK(t)         return (yylval.token = t)
    #define VAL_TOK(t)     yylval.string = new std::string(yytext, yyleng); return (t)
    #define STR_TOK(t, st) yylval.string = new std::string(st); return (t)
  #endif
#endif
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
if      TOK(IF);
else    TOK(ELSE);
while   TOK(WHILE);
new     TOK(NEW);
return  TOK(RETURN);
void    TOK(VOID);
int     TOK(INT);
char    TOK(CHAR);
float   TOK(FLOAT);
{HNUM}  VAL_TOK(NUMINT);
{LNUM}  VAL_TOK(NUMINT);
{DNUM}  VAL_TOK(NUMFLOAT);
{ID}    VAL_TOK(ID);
"("     TOK(APAR);
")"     TOK(FPAR);
"["     TOK(ACOL);
"]"     TOK(FCOL);
","     TOK(VIRG);
";"     TOK(PTVIRG);
"="     TOK(ATRIB);
"=="    TOK(IGUAL);
"+"     TOK(SOMA);
"-"     TOK(SUB);
"*"     TOK(MULT);
"/"     TOK(DIV);
">"     TOK(MAIORQ);
"<"     TOK(MENORQ);
">="    TOK(MAIORIG);
"<="    TOK(MENORIG);
"!"     TOK(NAO);
"||"    TOK(OU);
"&&"    TOK(E);
"{"     TOK(ACHAVE);
"}"     TOK(FCHAVE);
{WHITE} %{
#ifdef MG_SCANNER
            printf("%s", yytext); // output the spaces
#endif
        %}
.       %{
            mg_char_to_printable(yytext[0], (char *) mg_unexpected_char);
            mg_error("unexpected character '%s'", mg_unexpected_char);
        %}
}

<MG_COMMENT>{
"*/"      BEGIN(INITIAL);
[^*\n]+   // eat comments in chunks
"*"       // eat the lone star
\n        %{
#ifdef MG_SCANNER
              putchar('\n'); // eat newlines and output them
#endif
          %}
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
                  memcpy(&mg_str_buf[mg_str_buf_pos], yytext, yyleng);
                  mg_str_buf_pos += yyleng;
              %}
{NEWLINE}     mg_error("missing terminating character \""); BEGIN(INITIAL);
\"            %{
                  mg_ch_buffer_next_slot += mg_str_buf_pos;
                  BEGIN(INITIAL);
                  STR_TOK(STRING, mg_str_buf);
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
                  memcpy(&mg_str_buf[mg_str_buf_pos], yytext, yyleng);
                  mg_str_buf_pos += yyleng;
              %}
{NEWLINE}     mg_error("missing terminating character \""); BEGIN(INITIAL);
"'"           %{
                  mg_ch_buffer_next_slot += mg_str_buf_pos;
                  BEGIN(INITIAL);
                  STR_TOK(STRING, mg_str_buf);
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
    fprintf(stderr, "%d:error: ", yylineno);
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
