%{
    #include <string>
    #include <vector>

    #include "ast.h"
    /* raiz da AST */
    MongaProg* program;

    extern int yylex();
    void yyerror(const char *s) { printf("ERROR: %s\n", s); }
%}

/* representa um nó qualquer de nossa AST */
%union {
    MongaProg *prog;
    std::vector<MongaDecl*>* decls;
    MongaDecl* decl;
    MongaType* type;
    std::vector<MongaId*>* id_vec;
    std::vector<MongaArg*>* args;
    MongaArg* arg;
    MongaBlock* block;
    MongaStmt* stmt;
    MongaExp* exp;
    std::vector<MongaExp*> exp_vec;
    std::string *string;
    int token;
}

/* Define nossos símbolos terminais (tokens). O tipo pode ser qualquer campo da
 * union previamente definida */
%token <string> NUMINT NUMFLOAT STRING ID
%token <token> ELSE IF NEW RETURN VOID WHILE INT CHAR FLOAT
%token <token> APAR FPAR
%token <token> ACOL FCOL
%token <token> ACHAVE FCHAVE
%token <token> VIRG PTVIRG
%token <token> ATRIB IGUAL
%token <token> SOMA SUB MULT DIV
%token <token> MAIORQ MENORQ MAIORIG MENORIG
%token <token> NAO OU E

/* Define os símbolos não terminais. O tipo pode ser qualquer campo da union
 * previamente definida */
%type <prog> programa
%type <decls> declaracoes
%type <decl> declaracao dec_variaveis dec_funcao
%type <type> tipo
%type <token> tipo_base bracket_pairs
%type <id_vec> lista_nomes
%type <args> parametros parametros_nao_vazio
%type <arg> parametro
%type <block> bloco var_decls_or_stmts comando
%type <stmt> simple_stmt
%type <exp> var exp chamada
%type <exp_vec> lista_exp lista_exp_nao_vazia

/* Precedência dos operadores */
%left SOMA SUB
%left MULT DIV
%left IGUAL MENORIG MAIORIG MENORQ MAIORQ
%left E OU

%right ATRIB

%start programa

%%

programa : declaracoes { program = new MongaProg($<declaracoes>1); }
         ;

declaracoes : declaracao { $$ = new std::vector<MongaDecl>(); $$->push_back($<declaracao>1); }
            | declaracoes declaracao { $1->push_back($<declaracao>2); }
            ;

declaracao : dec_variaveis
           | dec_funcao
           ;

dec_variaveis : tipo lista_nomes PTVIRG { $$ = new MongaVarDecls($<tipo>1, $<lista_nomes>2); } 
              ;

tipo : tipo_base { $$ = new MongaType($<tipo_base>1); }
     | tipo_base bracket_pairs { $$ = new MongaType($<tipo_base>1, $<bracket_pairs>2); }
     ;

bracket_pairs : ACOL FCOL { $$ = 1; }
              | bracket_pairs ACOL FCOL { $$ = $1 + 1; }
              ;

tipo_base : INT
          | CHAR
          | FLOAT
          ;

lista_nomes : ID { $$ = new std::vector<MongaId>(); $$->push_back($1); }
            | lista_nomes VIRG ID { $1->push_back($3); }
            ;

dec_funcao : tipo ID APAR parametros FPAR bloco { $$ = new MongaFuncDecl($<tipo>1, $2, $<parametros>4, $<bloco>6); }
           | VOID ID APAR parametros FPAR bloco { $$ = new MongaFuncDecl(new MongaType($1), $2, $<parametros>4, $<bloco>6); }
           ;

parametros : parametros_nao_vazio
           | /* empty */ { $$ = new std::vector<MongaArg>(); }
           ;

parametros_nao_vazio : parametro { $$ = new std::vector<MongaArg>(); $$->push_back($<parametro>1); }
                     | parametros_nao_vazio VIRG parametro { $1->push_back($<parametro>3); }
                     ;

parametro : tipo ID { $$ = new MongaArg($<tipo>1, $2); }
          ;

bloco : ACHAVE var_decls_or_stmts FCHAVE { $$ = $<var_decls_or_stmts>2; }
      ;

var_decls_or_stmts : dec_variaveis { $$ = new MongaBlock(); $$->push_decl($<dec_variaveis>1); }
                   | simple_stmt { $$ = new MongaBlock(); $$->push_stmt($<simple_stmt>1); }
                   | var_decls_or_stmts dec_variaveis { $1->push_decl($<dec_variaveis>2); }
                   | var_decls_or_stmts simple_stmt { $1->push_stmt($<simple_stmt>2); }
                   ;

simple_stmt : IF APAR exp FPAR comando { $$ = new MongaIfStmt($<exp>3, $<comando>5); }
            | IF APAR exp FPAR comando ELSE comando { $$ = new MongaIfStmt($<exp>3, $<comando>5, $<comando>6); }
            | WHILE APAR exp FPAR comando { $$ = new MongaWhileStmt($<exp>3, $<comando>5); }
            | var ATRIB exp PTVIRG { $$ = new MongaAssignStmt($<var>$1, $<exp>3); }
            | RETURN PTVIRG { $$ = new MongaReturnStmt(); }
            | RETURN exp PTVIRG { $$ = new MongaReturnStmt($<exp>2); }
            | chamada PTVIRG { $$ = new MongaStmt($<chamada>1); }
            ;

comando : simple_stmt { $$ = new MongaBlock(); $$->push_stmt($<stmt>1); }
        | bloco
        ;

var : ID { $$ = new MongaVar($1); }
    | var ACOL exp FCOL { $$ = $<var>1->atAddress($<exp>3); }
    ;

exp : NUMINT { $$ = new MongaIntLiteral($1); }
    | NUMFLOAT { $$ = new MongaFloatLiteral($1); }
    | STRING { $$ = new MongaStringLiteral($1); }
    | var
    | APAR exp FPAR { $$ = $<exp>1; }
    | chamada
    | NEW tipo ACOL exp FCOL { $$ = new MongaNewStmtExp($<tipo>1, $<exp>2); }
    | SUB exp { $$ = new MongaMinusExp($<exp>2); }
    | exp SOMA exp { $$ = new MongaSumExp($<exp>1, $<exp>3); }
    | exp SUB exp { $$ = new MongaSubExp($<exp>1, $<exp>3); }
    | exp MULT exp { $$ = new MongaMultExp($<exp>1, $<exp>3); }
    | exp DIV exp { $$ = new MongaDivExp($<exp>1, $<exp>3); }
    | exp IGUAL exp { $$ = new MongaEqExp($<exp>1, $<exp>3); }
    | exp MENORIG exp { $$ = new MongaLowerEqExp($<exp>1, $<exp>3); }
    | exp MAIORIG exp { $$ = new MongaGreaterEqExp($<exp>1, $<exp>3); }
    | exp MENORQ exp { $$ = new MongaLowerExp($<exp>1, $<exp>3); }
    | exp MAIORQ exp { $$ = new MongaGreaterExp($<exp>1, $<exp>3); }
    | NAO exp { $$ = new MongaNotExp($<exp>1); }
    | exp E exp { $$ = new MongaAndExp($<exp>1, $<exp>3); }
    | exp OU exp { $$ = new MongaOrExp($<exp>1, $<exp>3); }
    ;

chamada : ID APAR lista_exp FPAR { $$ = new MongaFuncCall($1, $<lista_exp>3); }
        ;

lista_exp : lista_exp_nao_vazia
          | /* empty */ { $$ = new std::vector<MongaExp>(); }
          ;

lista_exp_nao_vazia : exp { $$ = new std::vector<MongaExp>(); $$->push_back(*$<exp>1); }
                    | lista_exp_nao_vazia VIRG exp { $$->push_back(*$<exp>3); }
                    ;

%%
