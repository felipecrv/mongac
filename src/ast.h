#ifndef MONGA_AST_
#define MONGA_AST_

#include <memory>
#include <sstream>
#include <vector>
#include <string>

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

template <typename T> class MongaVec;
typedef MongaVec<std::string> MongaIdVec;
typedef MongaVec<MongaArg> MongaArgsVec;
typedef MongaVec<MongaVarDeclsOrStmt> MongaBlock;
typedef MongaVec<MongaExp> MongaExpVec;
typedef MongaVec<MongaDecl> MongaProg;

class MongaAstNode {
    public:
        virtual string toStr() const = 0;
};

string toStr(const MongaAstNode& node);
string toStr(const string& s);

template <typename T>
class MongaVec : public MongaAstNode {
    public:
        vector<unique_ptr<T> > items;

        void add(T* item) {
            items.push_back(unique_ptr<T>(item));
        }

        virtual string toStr() const {
            auto it = items.begin();
            auto end = items.end();
            string s = "(";
            if (it != end) {
                s += monga::toStr(**it);
                for (it++; it != items.end(); it++) {
                    s += "\n" + monga::toStr(**it);
                }
            }
            return s + ")";
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

        string toStr() const;
};

class MongaArg : public MongaAstNode {
    private:
        unique_ptr<MongaType> type;
        unique_ptr<string> id;

    public:
        MongaArg(MongaType* type, string* id)
            : type(unique_ptr<MongaType>(type)), id(unique_ptr<string>(id)) {
        }

        string toStr() const { return "(arg " + type->toStr() + " " + *id + ")"; }
};

class MongaExp : public MongaAstNode {
};

class MongaIdentExp : public MongaExp {
    private:
        unique_ptr<string> id;

    public:
        MongaIdentExp(string* id) : id(unique_ptr<string>(id)) {
        }

        string toStr() const { return *id; }
};

class MongaIntLiteral : public MongaExp {
    private:
        long long val;

    public:
        MongaIntLiteral(string* s) {
            // TODO: convert
            // istringstream ss(*s);
            // ss >> val;
        }

        string toStr() const {
            string s = "_";
            // TODO: convert
            //ostringstream ss(s);
            //ss << val;
            return s;
        }
};

class MongaFloatLiteral : public MongaExp {
    private:
        double val;

    public:
        MongaFloatLiteral(string* s) {
            // TODO: convert s to double
            // val = atof(s->c_str());
        }

        string toStr() const {
            // TODO: convert
            return ".";
        }
};

class MongaStringLiteral : public MongaExp {
    private:
        unique_ptr<string> val;

    public:
        MongaStringLiteral(string* s) : val(unique_ptr<string>(s)) {
        }

        string toStr() const { return *val; }
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

        string toStr() const {
            return "(" + func_ident->toStr() + " " + arg_exps->toStr() + ")";
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

        virtual string operatorStr() const = 0;

        string toStr() const {
            return "(" + operatorStr() + " " + exp1->toStr() + " " + exp2->toStr() + ")";
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

        string toStr() const {
            return "(new " + type->toStr() + " " + exp->toStr() + ")";
        }
};

class MongaMinusExp : public MongaUnaryExp {
    public:
        MongaMinusExp(MongaExp* exp) : MongaUnaryExp(exp) {
        }

        string toStr() const { return "-" + exp->toStr(); }
};

class MongaSumExp : public MongaBinaryExp {
    private:
    public:
        MongaSumExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "+"; }
};

class MongaSubExp : public MongaBinaryExp {
    public:
        MongaSubExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "-"; }
};

class MongaMultExp : public MongaBinaryExp {
    public:
        MongaMultExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "*"; }
};

class MongaDivExp : public MongaBinaryExp {
    public:
        MongaDivExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "/"; }
};

class MongaEqExp : public MongaBinaryExp {
    public:
        MongaEqExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "=="; }
};

class MongaGreaterEqExp : public MongaBinaryExp {
    public:
        MongaGreaterEqExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return ">="; }
};

class MongaLowerExp : public MongaBinaryExp {
    public:
        MongaLowerExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "<"; }
};

class MongaGreaterExp : public MongaBinaryExp {
    public:
        MongaGreaterExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return ">"; }
};

class MongaLowerEqExp : public MongaBinaryExp {
    public:
        MongaLowerEqExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "<="; }
};

class MongaNotExp : public MongaUnaryExp {
    public:
        MongaNotExp(MongaExp* exp) : MongaUnaryExp(exp) {
        }

        string toStr() const { return "!" + exp->toStr(); }
};

class MongaAndExp : public MongaBinaryExp {
    public:
        MongaAndExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "&&"; }
};

class MongaOrExp : public MongaBinaryExp {
    public:
        MongaOrExp(MongaExp* exp1, MongaExp* exp2) : MongaBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "||"; }
};

class MongaVar : public MongaExp {
    private:
        unique_ptr<MongaExp> exp;
        MongaVec<MongaExp> arr_subscripts;

    public:
        MongaVar(MongaExp* exp) : exp(unique_ptr<MongaExp>(exp)) {
        }

        MongaVar(string* ident) : exp(unique_ptr<MongaExp>(new MongaIdentExp(ident))) {
        }

        MongaVar* push_subscript(MongaExp* subscript) {
            arr_subscripts.add(subscript);
            return this;
        }

        string toStr() const {
            if (arr_subscripts.items.size()) {
                return "(" + exp->toStr() + " " + arr_subscripts.toStr() + ")";
            }
            return exp->toStr();
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

        string toStr() const {
            return "(if " + cond_exp->toStr() + "\n" + then_block->toStr() +
                "\n" + else_block->toStr() + ")";
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
        
        string toStr() const {
            return "(while " + cond_exp->toStr() + "\n" + block->toStr() + ")";
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

        string toStr() const {
            return "(set " + var->toStr() + " " + rvalue->toStr() + ")";
        }
};

class MongaReturnStmt : public MongaStmt {
    private:
        unique_ptr<MongaExp> exp;

    public:
        MongaReturnStmt(MongaExp* exp = NULL) : exp(unique_ptr<MongaExp>(exp)) {
        }

        string toStr() const { return "(ret " + exp->toStr() + ")"; }
};

class MongaExpStmt : public MongaStmt {
    private:
        unique_ptr<MongaExp> exp;

    public:
        MongaExpStmt(MongaExp* exp) : exp(unique_ptr<MongaExp>(exp)) {
        }

        string toStr() const { return exp->toStr(); }
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

        string toStr() const {
            string s = "(var " + type->toStr();
            for (auto it = idents->items.begin(); it != idents->items.end(); it++) {
                s += " " + **it;
            }
            return s + ")";
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

        string toStr() const {
            return "(fun " + type->toStr() + " " + *id + "\n" + args->toStr()
                + "\n" + monga::toStr(*block) + ")";
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

        string toStr() const { return node->toStr(); }
};

}; // namespace monga

#endif // MONGA_AST_
