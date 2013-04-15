%{
    #include <string>
    #include <vector>
    #include "tokens.h"
    #include "ast.h"
    using namespace monga;

    /* raiz da AST */
    Prog* program;

    void yyerror(const char *s) {
        fprintf(stderr, "%d:error: %s\n", yylineno, s);
    }
%}

%locations
%error-verbose

/* representa um nó qualquer de nossa AST */
%union {
    Prog *prog;
    Decl* decl;
    VarDecl* var_decl;
    VarDeclVec* var_decl_vec;
    Type* type;
    IdVec* id_vec;
    Arg* arg;
    ArgsVec* args;
    Stmt* stmt;
    Command* command;
    CommandVec* command_vec;
    Block* block;
    Exp* exp;
    ExpVec* exp_vec;
    FuncCallExp* func_call_exp;
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
%right ATRIB
%left OU
%left E
%left IGUAL
%left MENORQ MENORIG MAIORQ MAIORIG
%left SOMA SUB
%left MULT DIV
%nonassoc NAO

%start programa

%%

programa : /* empty */ { $$ = program = new Prog(); }
         | programa declaracao { $<prog>1->add($<decl>2); }
         ;

declaracao : dec_variaveis { $$ = $<var_decl>1; }
           | dec_funcao
           ;

dec_variaveis : tipo lista_nomes PTVIRG { $$ = new VarDecl($<type>1, $<id_vec>2); $$->setLineno(@1.first_line); }
              ;

tipo : tipo_base { $$ = new Type($1); $$->setLineno(@1.first_line); }
     | tipo ACOL FCOL { $<type>1->addArrDim(); }
     ;

tipo_base : INT
          | CHAR
          | FLOAT
          ;

lista_nomes : ID { $$ = new IdVec(); $$->add($1); $$->setLineno(@1.first_line); }
            | lista_nomes VIRG ID { $<id_vec>1->add($3); }
            ;

dec_funcao : tipo ID APAR parametros FPAR bloco { $$ = new FuncDecl($<type>1, $2, $<args>4, $<block>6); $$->setLineno(@2.first_line); }
           | VOID ID APAR parametros FPAR bloco { $$ = new FuncDecl(new Type($1), $2, $<args>4, $<block>6); $$->setLineno(@2.first_line); }
           ;

parametros : /* empty */ { $$ = new ArgsVec(); }
           | parametros_nao_vazio
           ;

parametros_nao_vazio : parametro { $$ = new ArgsVec(); $$->add($<arg>1); $$->setLineno(@1.first_line); }
                     | parametros_nao_vazio VIRG parametro { $1->add($<arg>3); }
                     ;

parametro : tipo ID { $$ = new Arg($<type>1, $2); $$->setLineno(@1.first_line); }
          ;

bloco : ACHAVE local_var_decls comandos FCHAVE { $$ = new Block($<var_decl_vec>2, $<command_vec>3); }
      ;

local_var_decls : /* empty */ { $$ = new VarDeclVec(); }
                | local_var_decls dec_variaveis { $<var_decl_vec>1->add($<var_decl>2); }
                ;

comandos : /* empty */ { $$ = new CommandVec(); }
         | comandos comando { $<command_vec>1->add($<command>2); }
         ;

comando : stmt { $$ = new Command($<stmt>1); $$->setLineno(@1.first_line); }
        ;

stmt : IF APAR exp FPAR comando { $$ = new IfStmt($<exp>3, $<command>5); $$->setLineno(@1.first_line); }
     | IF APAR exp FPAR comando ELSE comando { $$ = new IfStmt($<exp>3, $<command>5, $<command>7); $$->setLineno(@1.first_line); }
     | WHILE APAR exp FPAR comando { $$ = new WhileStmt($<exp>3, $<command>5); $$->setLineno(@1.first_line); }
     | var ATRIB exp PTVIRG { $$ = new AssignStmt((Var*) $<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
     | RETURN PTVIRG { $$ = new ReturnStmt(); $$->setLineno(@1.first_line); }
     | RETURN exp PTVIRG { $$ = new ReturnStmt($<exp>2); $$->setLineno(@1.first_line); }
     | chamada PTVIRG { $$ = new FuncCallStmt($<func_call_exp>1); $$->setLineno(@1.first_line); }
     | bloco { $$ = new BlockStmt($<block>1); }
     ;

var : ID { $$ = new Var($1); $$->setLineno(@1.first_line); }
    | var ACOL exp FCOL { $$ = ((Var *) $<exp>1)->push_subscript($<exp>3); $$->setLineno(@1.first_line); }
    ;

exp : NUMINT { $$ = new IntLiteral($<string>1); $$->setLineno(@1.first_line); }
    | NUMFLOAT { string s(*$1);
                 if (s[0] == '.') {
                   s = "0" + s;
                 } else if (s[s.size() - 1] == '.') {
                   s = s + "0";
                 }
                 $$ = new FloatLiteral(&s);
				 $$->setLineno(@1.first_line);
               }
    | STRING { $$ = new StringLiteral($<string>1); $$->setLineno(@1.first_line); }
    | var
    | APAR exp FPAR { $$ = $<exp>2; }
    | chamada { $$ = $<func_call_exp>1; }
    | NEW tipo ACOL exp FCOL { $$ = new NewExp($<type>2, $<exp>4); $$->setLineno(@1.first_line); }
    | SUB exp { $$ = new MinusExp($<exp>2); $$->setLineno(@1.first_line); }
    | exp SOMA exp { $$ = new SumExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp SUB exp { $$ = new SubExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp MULT exp { $$ = new MultExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp DIV exp { $$ = new DivExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp IGUAL exp { $$ = new EqExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp MENORIG exp { $$ = new LowerEqExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp MAIORIG exp { $$ = new GreaterEqExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp MENORQ exp { $$ = new LowerExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp MAIORQ exp { $$ = new GreaterExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | NAO exp { $$ = new NotExp($<exp>2); $$->setLineno(@1.first_line); }
    | exp E exp { $$ = new AndExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    | exp OU exp { $$ = new OrExp($<exp>1, $<exp>3); $$->setLineno(@2.first_line); }
    ;

chamada : ID APAR lista_exp FPAR { $$ = new FuncCallExp($1, $<exp_vec>3); $$->setLineno(@1.first_line); }
        ;

lista_exp : /* empty */ { $$ = new ExpVec(); }
          | lista_exp_nao_vazia
          ;

lista_exp_nao_vazia : exp { $$ = new ExpVec(); $$->add($<exp>1); }
                    | lista_exp_nao_vazia VIRG exp { $$->add($<exp>3); }
                    ;

%%
