%{
    #include <string>
    #include <vector>
    #include "tokens.h"
    #include "ast.h"
    using namespace monga;

    /* raiz da AST */
    MongaProg* program;

    void yyerror(const char *s) {
        fprintf(stderr, "%d:error: %s\n", yylineno, s);
    }
%}

%error-verbose

/* representa um nó qualquer de nossa AST */
%union {
    MongaProg *prog;
    MongaDecl* decl;
    MongaVarDecl* var_decl;
    MongaVarDeclVec* var_decl_vec;
    MongaType* type;
    MongaIdVec* id_vec;
    MongaArg* arg;
    MongaArgsVec* args;
    MongaStmt* stmt;
    MongaCommand* command;
    MongaCommandVec* command_vec;
    MongaBlock* block;
    MongaExp* exp;
    MongaExpVec* exp_vec;
    MongaFuncCallExp* func_call_exp;
    std::string* string;
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
%type <decl> declaracao dec_funcao
%type <var_decl> dec_variaveis
%type <type> tipo
%type <token> tipo_base
%type <id_vec> lista_nomes
%type <args> parametros parametros_nao_vazio
%type <arg> parametro
%type <block> bloco
%type <var_decl_vec> local_var_decls
%type <stmt> stmt
%type <command> comando
%type <command_vec> comandos
%type <exp> exp var
%type <func_call_exp> chamada
%type <exp_vec> lista_exp lista_exp_nao_vazia

/* Precedência dos operadores */
%left SOMA SUB
%left MULT DIV
%left E OU
%left IGUAL MENORIG MAIORIG MENORQ MAIORQ
%right ATRIB

%nonassoc NAO

%start programa

%%

programa : /* empty */ { $$ = program = new MongaProg(); }
         | programa declaracao { $<prog>1->add($<decl>2); }
         ;

declaracao : dec_variaveis { $$ = $<var_decl>1; }
           | dec_funcao
           ;

dec_variaveis : tipo lista_nomes PTVIRG { $$ = new MongaVarDecl($<type>1, $<id_vec>2); }
              ;

tipo : tipo_base { $$ = new MongaType($1); }
     | tipo ACOL FCOL { $<type>1->addDimension(); $$ = $<type>1; }
     ;

tipo_base : INT
          | CHAR
          | FLOAT
          ;

lista_nomes : ID { $$ = new MongaIdVec(); $$->add($1); }
            | lista_nomes VIRG ID { $<id_vec>1->add($3); }
            ;

dec_funcao : tipo ID APAR parametros FPAR bloco { $$ = new MongaFuncDecl($<type>1, $2, $<args>4, $<block>6); }
           | VOID ID APAR parametros FPAR bloco { $$ = new MongaFuncDecl(new MongaType($1), $2, $<args>4, $<block>6); }
           ;

parametros : /* empty */ { $$ = new MongaArgsVec(); }
           | parametros_nao_vazio
           ;

parametros_nao_vazio : parametro { $$ = new MongaArgsVec(); $$->add($<arg>1); }
                     | parametros_nao_vazio VIRG parametro { $1->add($<arg>3); }
                     ;

parametro : tipo ID { $$ = new MongaArg($<type>1, $2); }
          ;

bloco : ACHAVE local_var_decls comandos FCHAVE { $$ = new MongaBlock($<var_decl_vec>2, $<command_vec>3); }
      ;

local_var_decls : /* empty */ { $$ = new MongaVarDeclVec(); }
                | local_var_decls dec_variaveis { $<var_decl_vec>1->add($<var_decl>2); }
                ;

comandos : /* empty */ { $$ = new MongaCommandVec(); }
         | comandos comando { $<command_vec>1->add($<command>2); }
         ;

comando : stmt { $$ = new MongaCommand($<stmt>1); }
        ;

stmt : IF APAR exp FPAR comando { $$ = new MongaIfStmt($<exp>3, $<command>5); }
     | IF APAR exp FPAR comando ELSE comando { $$ = new MongaIfStmt($<exp>3, $<command>5, $<command>7); }
     | WHILE APAR exp FPAR comando { $$ = new MongaWhileStmt($<exp>3, $<block>5); }
     | var ATRIB exp PTVIRG { $$ = new MongaAssignStmt((MongaVar*) $<exp>1, $<exp>3); }
     | RETURN PTVIRG { $$ = new MongaReturnStmt(); }
     | RETURN exp PTVIRG { $$ = new MongaReturnStmt($<exp>2); }
     | chamada PTVIRG { $$ = new MongaFuncCallStmt($<func_call_exp>1); }
     | bloco { $$ = new MongaBlockStmt($<block>1); }
     ;

var : ID { $$ = new MongaVar($1); }
    | var ACOL exp FCOL { $$ = ((MongaVar *) $<exp>1)->push_subscript($<exp>3); }
    ;

exp : NUMINT { $$ = new MongaIntLiteral($<string>1); }
    | NUMFLOAT { string s(*$1);
                 if (s[0] == '.') {
                   s = "0" + s;
                 } else if (s[s.size() - 1] == '.') {
                   s = s + "0";
                 }
                 $$ = new MongaFloatLiteral(&s);
               }
    | STRING { $$ = new MongaStringLiteral($<string>1); }
    | var
    | APAR exp FPAR { $$ = $<exp>2; }
    | chamada { $$ = $<func_call_exp>1; }
    | NEW tipo ACOL exp FCOL { $$ = new MongaNewStmtExp($<type>2, $<exp>4); }
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
    | NAO exp { $$ = new MongaNotExp($<exp>2); }
    | exp E exp { $$ = new MongaAndExp($<exp>1, $<exp>3); }
    | exp OU exp { $$ = new MongaOrExp($<exp>1, $<exp>3); }
    ;

chamada : ID APAR lista_exp FPAR { $$ = new MongaFuncCallExp($1, $<exp_vec>3); }
        ;

lista_exp : /* empty */ { $$ = new MongaExpVec(); }
          | lista_exp_nao_vazia
          ;

lista_exp_nao_vazia : exp { $$ = new MongaExpVec(); $$->add($<exp>1); }
                    | lista_exp_nao_vazia VIRG exp { $$->add($<exp>3); }
                    ;

%%
