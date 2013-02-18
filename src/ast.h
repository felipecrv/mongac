#ifndef MONGA_AST_
#define MONGA_AST_

#include <memory>
#include <boost/lexical_cast.hpp>

namespace monga {
using namespace std;

class MongaAstNode;
class MongaArg;
class MongaExp;
class MongaIdentExp;
class MongaVar;
class MongaStmt;
class MongaVarDeclsOrStmt;
class MongaIfStmt;
class MongaWhileStmt;
class MongaAssignStmt;
class MongaReturnStmt;
class MongaExpStmt;
class MongaIntLiteral;
class MongaFloatLiteral;
class MongaStringLiteral;
class MongaFuncCall;
class MongaBinaryExp;
class MongaUnaryExp;
class MongaNewStmtExp;
class MongaMinusExp;
class MongaSumExp;
class MongaSubExp;
class MongaMultExp;
class MongaDivExp;
class MongaEqExp;
class MongaGreaterEqExp;
class MongaLowerExp;
class MongaGreaterExp;
class MongaLowerEqExp;
class MongaNotExp;
class MongaAndExp;
class MongaOrExp;
class MongaType;
class MongaDecl;
class MongaVarDecls;
class MongaFuncDecl;
class MongaProg;

template <typename T> class MongaVec;
typedef MongaVec<string> MongaIdVec;
typedef MongaVec<MongaArg> MongaArgsVec;
typedef MongaVec<MongaVarDeclsOrStmt> MongaBlock;
typedef MongaVec<MongaExp> MongaExpVec;

class MongaAstNode {
};

template <typename T>
class MongaVec : public MongaAstNode {
    private:
        vector<unique_ptr<T> > items;

    public:
        MongaVec() {

        }

        void push_back(T* item) {
            items.push_back(unique_ptr<T>(item));
        }
};

class MongaType : public MongaAstNode {
    private:
        int type_tok;
        int array_dimensions;

    public:
        MongaType(int type_tok, int arr_dimensions = 0)
            : type_tok(type_tok), array_dimensions(arr_dimensions) {
        }

        void addDimension() {
            ++array_dimensions;
        }
};

class MongaArg : public MongaAstNode {
    private:
        unique_ptr<MongaType> type;
        unique_ptr<string> ident;

    public:
        MongaArg(MongaType* type, string* ident)
            : type(unique_ptr<MongaType>(type)), ident(unique_ptr<string>(ident)) {
        }
};

class MongaExp : public MongaAstNode {
};

class MongaIdentExp : public MongaExp {
    private:
        unique_ptr<string> ident;

    public:
        MongaIdentExp(string* ident) : ident(unique_ptr<string>(ident)) {
        }
};

class MongaIntLiteral : public MongaExp {
    private:
        long long val;

    public:
        MongaIntLiteral(string* s)
            : val(boost::lexical_cast<long long>(*s)) {
        }
};

class MongaFloatLiteral : public MongaExp {
    private:
        double val;

    public:
        MongaFloatLiteral(string* s) : val(boost::lexical_cast<double>(*s)) {
        }
};

class MongaStringLiteral : public MongaExp {
    private:
        unique_ptr<string> val;

    public:
        MongaStringLiteral(string* s) : val(unique_ptr<string>(s)) {
        }
};

class MongaFuncCall : public MongaExp {
    private:
        unique_ptr<MongaIdentExp> func_ident;
        unique_ptr<MongaExpVec> arg_exps;

    public:
        MongaFuncCall(MongaIdentExp* func_ident, MongaExpVec* arg_exps)
            : func_ident(unique_ptr<MongaIdentExp>(func_ident)),
            arg_exps(unique_ptr<MongaExpVec>(arg_exps)) {
        }

        MongaFuncCall(string* func_ident, MongaExpVec* arg_exps)
            : MongaFuncCall(new MongaIdentExp(func_ident), arg_exps) {
        }
};

class MongaBinaryExp : public MongaExp {
    private:
        unique_ptr<MongaExp> exp1;
        unique_ptr<MongaExp> exp2;

    public:
        MongaBinaryExp(MongaExp* exp1, MongaExp* exp2)
            : exp1(unique_ptr<MongaExp>(exp1)), exp2(unique_ptr<MongaExp>(exp2)) {
        }
};

class MongaUnaryExp : public MongaExp {
    protected:
        unique_ptr<MongaExp> exp;

    public:
        MongaUnaryExp(MongaExp* exp) : exp(unique_ptr<MongaExp>(exp)) {
        }
};

class MongaNewStmtExp : public MongaExp {
    private:
        unique_ptr<MongaType> type;
        unique_ptr<MongaExp> exp;

    public:
        MongaNewStmtExp(MongaType* type, MongaExp* exp)
            : type(unique_ptr<MongaType>(type)), exp(unique_ptr<MongaExp>(exp)) {
        }
};

class MongaMinusExp : public MongaUnaryExp {
    public:
        MongaMinusExp(MongaExp* exp) : MongaUnaryExp(exp) {
        }
};

class MongaSumExp : public MongaBinaryExp {
    private:
    public:
        MongaSumExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaSubExp : public MongaBinaryExp {
    public:
        MongaSubExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaMultExp : public MongaBinaryExp {
    public:
        MongaMultExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaDivExp : public MongaBinaryExp {
    public:
        MongaDivExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaEqExp : public MongaBinaryExp {
    public:
        MongaEqExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaGreaterEqExp : public MongaBinaryExp {
    public:
        MongaGreaterEqExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaLowerExp : public MongaBinaryExp {
    public:
        MongaLowerExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaGreaterExp : public MongaBinaryExp {
    public:
        MongaGreaterExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaLowerEqExp : public MongaBinaryExp {
    public:
        MongaLowerEqExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaNotExp : public MongaUnaryExp {
    public:
        MongaNotExp(MongaExp* exp) : MongaUnaryExp(exp) {
        }
};

class MongaAndExp : public MongaBinaryExp {
    public:
        MongaAndExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaOrExp : public MongaBinaryExp {
    public:
        MongaOrExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }
};

class MongaVar : public MongaExp {
    private:
        unique_ptr<MongaExp> exp;
        vector<unique_ptr<MongaExp> > arr_subscripts;

    public:
        MongaVar(MongaExp* exp) : exp(unique_ptr<MongaExp>(exp)) {
        }

        MongaVar(string* ident) : exp(unique_ptr<MongaExp>(new MongaIdentExp(ident))) {
        }

        MongaVar* push_subscript(MongaExp* subscript) {
            arr_subscripts.push_back(unique_ptr<MongaExp>(subscript));
            return this;
        }
};

class MongaStmt : public MongaAstNode {
};

class MongaIfStmt : public MongaStmt {
    private:
        unique_ptr<MongaExp> cond_exp;
        unique_ptr<MongaBlock> then_block;
        unique_ptr<MongaBlock> else_block;

    public:
        MongaIfStmt(MongaExp* cond_exp, MongaBlock* then_block, MongaBlock* else_block = NULL)
            : cond_exp(unique_ptr<MongaExp>(cond_exp)),
            then_block(unique_ptr<MongaBlock>(then_block)),
            else_block(unique_ptr<MongaBlock>(else_block)) {
        }
};

class MongaWhileStmt : public MongaStmt {
    private:
        unique_ptr<MongaExp> cond_exp;
        unique_ptr<MongaBlock> block;

    public:
        MongaWhileStmt(MongaExp* cond_exp, MongaBlock* block)
            : cond_exp(unique_ptr<MongaExp>(cond_exp)),
            block(unique_ptr<MongaBlock>(block)) {
        }
};

class MongaAssignStmt : public MongaStmt {
    private:
        unique_ptr<MongaVar> var;
        unique_ptr<MongaExp> rvalue;

    public:
        MongaAssignStmt(MongaVar* var, MongaExp* rvalue)
            : var(unique_ptr<MongaVar>(var)),
            rvalue(unique_ptr<MongaExp>(rvalue)) {
        }
};

class MongaReturnStmt : public MongaStmt {
    private:
        unique_ptr<MongaExp> exp;

    public:
        MongaReturnStmt(MongaExp* exp = NULL) : exp(unique_ptr<MongaExp>(exp)) {
        }
};

class MongaExpStmt : public MongaStmt {
    private:
        unique_ptr<MongaExp> exp;

    public:
        MongaExpStmt(MongaExp* exp) : exp(unique_ptr<MongaExp>(exp)) {
        }
};

class MongaDecl : public MongaAstNode {
};

class MongaVarDecls : public MongaDecl {
    private:
        unique_ptr<MongaType> type;
        unique_ptr<MongaIdVec> idents;

    public:
        MongaVarDecls(MongaType* type, MongaIdVec* idents)
            : type(unique_ptr<MongaType>(type)), idents(unique_ptr<MongaIdVec>(idents)) {
        }
};

class MongaFuncDecl : public MongaDecl {
    private:
        unique_ptr<MongaType> type;
        unique_ptr<string> id;
        unique_ptr<MongaArgsVec> args;
        unique_ptr<MongaBlock> block;

    public:
        MongaFuncDecl(MongaType* type, string* id, MongaArgsVec* args, MongaBlock* block)
            : type(unique_ptr<MongaType>(type)), id(unique_ptr<string>(id)),
            args(unique_ptr<MongaArgsVec>(args)), block(unique_ptr<MongaBlock>(block)) {
        }
};

class MongaVarDeclsOrStmt : public MongaAstNode {
    private:
        bool is_decl;
        unique_ptr<MongaAstNode> node;

    public:
        MongaVarDeclsOrStmt(MongaVarDecls* decls)
            : is_decl(true), node(unique_ptr<MongaVarDecls>(decls)) {
        }

        MongaVarDeclsOrStmt(MongaStmt* stmt) {
            is_decl = false;
            node = unique_ptr<MongaStmt>(stmt);
        }
};

class MongaProg : public MongaAstNode {
    private:
        vector<unique_ptr<MongaDecl> > decls;

    public:
        MongaProg() {
        }

        void push_back(MongaDecl* decl) {
            decls.push_back(unique_ptr<MongaDecl>(decl));
        }
};

}; // namespace monga

#endif // MONGA_AST_
