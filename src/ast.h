#ifndef MONGA_AST_
#define MONGA_AST_

#include <memory>
#include <sstream>
#include <vector>
#include <string>

namespace monga {
using namespace std;

class AstNode;
class Arg;
class Exp;
class IdentExp;
class Var;
class Stmt;
class IfStmt;
class WhileStmt;
class AssignStmt;
class ReturnStmt;
class ExpStmt;
class IntLiteral;
class FloatLiteral;
class StringLiteral;
class FuncCallExp;
class BinaryExp;
class UnaryExp;
class NewStmtExp;
class MinusExp;
class SumExp;
class SubExp;
class MultExp;
class DivExp;
class EqExp;
class GreaterEqExp;
class LowerExp;
class GreaterExp;
class LowerEqExp;
class NotExp;
class AndExp;
class OrExp;
class Type;
class Decl;
class VarDecl;
class FuncDecl;
class Block;
class Command;

template <typename T> class Vec;
typedef Vec<std::string> IdVec;
typedef Vec<Arg> ArgsVec;
typedef Vec<Exp> ExpVec;
typedef Vec<VarDecl> VarDeclVec;
typedef Vec<Command> CommandVec;

typedef Vec<Decl> Prog;

class AstNode {
    public:
        virtual string toStr() const = 0;
};

string toStr(const AstNode& node);
string toStr(const string& s);

template <typename T>
class Vec : public AstNode {
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

class Type : public AstNode {
    private:
        int type_tok;
        int array_dimensions;

    public:
        Type(int type_tok, int arr_dimensions = 0)
            : type_tok(type_tok), array_dimensions(arr_dimensions) {
        }

        void addDimension() {
            ++array_dimensions;
        }

        string toStr() const;
};

class Arg : public AstNode {
    private:
        unique_ptr<Type> type;
        unique_ptr<string> id;

    public:
        Arg(Type* type, string* id)
            : type(unique_ptr<Type>(type)), id(unique_ptr<string>(id)) {
        }

        string toStr() const { return "(arg " + type->toStr() + " " + *id + ")"; }
};

class Exp : public AstNode {
};

class IdentExp : public Exp {
    private:
        unique_ptr<string> id;

    public:
        IdentExp(string* id) : id(unique_ptr<string>(id)) {
        }

        string toStr() const { return *id; }
};

class IntLiteral : public Exp {
    private:
        long long val;

    public:
        IntLiteral(string* s) {
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

class FloatLiteral : public Exp {
    private:
        double val;

    public:
        FloatLiteral(string* s) {
            // TODO: convert s to double
            // val = atof(s->c_str());
        }

        string toStr() const {
            // TODO: convert
            return ".";
        }
};

class StringLiteral : public Exp {
    private:
        unique_ptr<string> val;

    public:
        StringLiteral(string* s) : val(unique_ptr<string>(s)) {
        }

        string toStr() const { return *val; }
};

class FuncCallExp : public Exp {
    private:
        unique_ptr<IdentExp> func_ident;
        unique_ptr<ExpVec> arg_exps;

    public:
        FuncCallExp(IdentExp* func_ident, ExpVec* arg_exps)
            : func_ident(unique_ptr<IdentExp>(func_ident)),
            arg_exps(unique_ptr<ExpVec>(arg_exps)) {
        }

        FuncCallExp(string* func_ident, ExpVec* arg_exps)
            : FuncCallExp(new IdentExp(func_ident), arg_exps) {
        }

        string toStr() const {
            return "(" + func_ident->toStr() + " " + arg_exps->toStr() + ")";
        }
};

class BinaryExp : public Exp {
    private:
        unique_ptr<Exp> exp1;
        unique_ptr<Exp> exp2;

    public:
        BinaryExp(Exp* exp1, Exp* exp2)
            : exp1(unique_ptr<Exp>(exp1)), exp2(unique_ptr<Exp>(exp2)) {
        }

        virtual string operatorStr() const = 0;

        string toStr() const {
            return "(" + operatorStr() + " " + exp1->toStr() + " " + exp2->toStr() + ")";
        }
};

class UnaryExp : public Exp {
    protected:
        unique_ptr<Exp> exp;

    public:
        UnaryExp(Exp* exp) : exp(unique_ptr<Exp>(exp)) {
        }
};

class NewStmtExp : public Exp {
    private:
        unique_ptr<Type> type;
        unique_ptr<Exp> exp;

    public:
        NewStmtExp(Type* type, Exp* exp)
            : type(unique_ptr<Type>(type)), exp(unique_ptr<Exp>(exp)) {
        }

        string toStr() const {
            return "(new " + type->toStr() + " " + exp->toStr() + ")";
        }
};

class MinusExp : public UnaryExp {
    public:
        MinusExp(Exp* exp) : UnaryExp(exp) {
        }

        string toStr() const { return "-" + exp->toStr(); }
};

class SumExp : public BinaryExp {
    private:
    public:
        SumExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "+"; }
};

class SubExp : public BinaryExp {
    public:
        SubExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "-"; }
};

class MultExp : public BinaryExp {
    public:
        MultExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "*"; }
};

class DivExp : public BinaryExp {
    public:
        DivExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "/"; }
};

class EqExp : public BinaryExp {
    public:
        EqExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "=="; }
};

class GreaterEqExp : public BinaryExp {
    public:
        GreaterEqExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return ">="; }
};

class LowerExp : public BinaryExp {
    public:
        LowerExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "<"; }
};

class GreaterExp : public BinaryExp {
    public:
        GreaterExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return ">"; }
};

class LowerEqExp : public BinaryExp {
    public:
        LowerEqExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "<="; }
};

class NotExp : public UnaryExp {
    public:
        NotExp(Exp* exp) : UnaryExp(exp) {
        }

        string toStr() const { return "!" + exp->toStr(); }
};

class AndExp : public BinaryExp {
    public:
        AndExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "&&"; }
};

class OrExp : public BinaryExp {
    public:
        OrExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "||"; }
};

class Var : public Exp {
    private:
        unique_ptr<Exp> exp;
        Vec<Exp> arr_subscripts;

    public:
        Var(Exp* exp) : exp(unique_ptr<Exp>(exp)) {
        }

        Var(string* ident) : exp(unique_ptr<Exp>(new IdentExp(ident))) {
        }

        Var* push_subscript(Exp* subscript) {
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

class Block : public AstNode {
    private:
        unique_ptr<VarDeclVec> vars;
        unique_ptr<CommandVec> commands;

    public:
        Block(VarDeclVec* var_decls, CommandVec* commands)
            : vars(unique_ptr<VarDeclVec>(var_decls)),
            commands(unique_ptr<CommandVec>(commands)) {
        }

        string toStr() const {
            return "(" + vars->toStr() + "\n" + commands->toStr() + ")";
        }
};

class Stmt : public AstNode {
};

class BlockStmt : public Stmt {
    private:
        unique_ptr<Block> block;

    public:
        BlockStmt(Block* block) : block(unique_ptr<Block>(block)) {
        }

        string toStr() const {
            return block->toStr();
        }
};

class Command : public AstNode {
    private:
        unique_ptr<Stmt> stmt;

    public:
        Command(Stmt* stmt) : stmt(unique_ptr<Stmt>(stmt)) {
        }

        Command(Block* block)
            : stmt(unique_ptr<BlockStmt>(new BlockStmt(block))) {
        }

        string toStr() const {
            return stmt->toStr();
        }
};

class IfStmt : public Stmt {
    private:
        unique_ptr<Exp> cond_exp;
        unique_ptr<Command> then_cmd;
        unique_ptr<Command> else_cmd;

    public:
        IfStmt(Exp* cond_exp, Command* then_cmd, Command* else_cmd = NULL)
            : cond_exp(unique_ptr<Exp>(cond_exp)),
            then_cmd(unique_ptr<Command>(then_cmd)),
            else_cmd(unique_ptr<Command>(else_cmd)) {
        }

        string toStr() const {
            string s = "(if " + cond_exp->toStr() + "\n" + then_cmd->toStr();
            if (else_cmd) {
                s += "\n" + else_cmd->toStr();
            }
            return s + ")";
        }
};

class WhileStmt : public Stmt {
    private:
        unique_ptr<Exp> cond_exp;
        unique_ptr<Block> block;

    public:
        WhileStmt(Exp* cond_exp, Block* block)
            : cond_exp(unique_ptr<Exp>(cond_exp)),
            block(unique_ptr<Block>(block)) {
        }
        
        string toStr() const {
            return "(while " + cond_exp->toStr() + "\n" + block->toStr() + ")";
        }
};

class AssignStmt : public Stmt {
    private:
        unique_ptr<Var> var;
        unique_ptr<Exp> rvalue;

    public:
        AssignStmt(Var* var, Exp* rvalue)
            : var(unique_ptr<Var>(var)),
            rvalue(unique_ptr<Exp>(rvalue)) {
        }

        string toStr() const {
            return "(set " + var->toStr() + " " + rvalue->toStr() + ")";
        }
};

class ReturnStmt : public Stmt {
    private:
        unique_ptr<Exp> exp;

    public:
        ReturnStmt(Exp* exp = NULL) : exp(unique_ptr<Exp>(exp)) {
        }

        string toStr() const { 
            if (exp == NULL) {
                return "ret";
            }
            return "(ret " + exp->toStr() + ")"; }
};

class FuncCallStmt : public Stmt {
    private:
        unique_ptr<FuncCallExp> exp;

    public:
        FuncCallStmt(FuncCallExp* exp)
            : exp(unique_ptr<FuncCallExp>(exp)) {
        }

        string toStr() const { return exp->toStr(); }
};

class Decl : public AstNode {
};

class VarDecl : public Decl {
    private:
        unique_ptr<Type> type;
        unique_ptr<IdVec> idents;

    public:
        VarDecl(Type* type, IdVec* idents)
            : type(unique_ptr<Type>(type)), idents(unique_ptr<IdVec>(idents)) {
        }

        string toStr() const {
            string s = "(var " + type->toStr();
            for (auto it = idents->items.begin(); it != idents->items.end(); it++) {
                s += " " + **it;
            }
            return s + ")";
        }
};

class FuncDecl : public Decl {
    private:
        unique_ptr<Type> type;
        unique_ptr<string> id;
        unique_ptr<ArgsVec> args;
        unique_ptr<Block> block;

    public:
        FuncDecl(Type* type, string* id, ArgsVec* args, Block* block)
            : type(unique_ptr<Type>(type)), id(unique_ptr<string>(id)),
            args(unique_ptr<ArgsVec>(args)), block(unique_ptr<Block>(block)) {
        }

        string toStr() const {
            return "(fun " + type->toStr() + " " + *id + "\n" + args->toStr()
                + "\n" + monga::toStr(*block) + ")";
        }
};

}; // namespace monga

#endif // MONGA_AST_
