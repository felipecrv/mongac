#ifndef MONGA_AST_
#define MONGA_AST_

#include <iostream>
#include <memory>
#include <sstream>
#include <vector>
#include <string>
#include <map>
#include "exception.h"

namespace monga {
using namespace std;

#ifdef DEBUG
  #define LOG(os_ops)  std::cout << __FILE__ << ":" << __LINE__ << ": " << os_ops << std::endl
#else
  #define LOG(os_ops)
#endif

class AstNode;
class Type;
class Env;
class Arg;
class Exp;
class IdentExp;
class IntLiteral;
class FloatLiteral;
class StringLiteral;
class FuncCallExp;
class BinaryExp;
class UnaryExp;
class NewExp;
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
class Var;
class Decl;
class VarDecl;
class Block;
class Stmt;
class Command;
class IfStmt;
class WhileStmt;
class AssignStmt;
class ReturnStmt;
class FuncDecl;

template <typename T> class Vec;
typedef Vec<string> IdVec;
typedef Vec<Type> TypeVec;
typedef Vec<Arg> ArgsVec;
typedef Vec<Exp> ExpVec;
typedef Vec<Command> CommandVec;

shared_ptr<Type> the_void_type();

class AstNode {
    public:
        int lineno;

        AstNode() : lineno(-1) {}
        virtual string toStr() const = 0;
        virtual shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>) {
            return the_void_type();
        }
        void setLineno(int ln) { lineno = ln; }
};

string toStr(const AstNode& node);
string toStr(const string& s);
Type if_stmt_type(Type, Type);

class Type : public AstNode {
    friend class Var;

    private:
        int type_tok;
        int arr_dim;

    public:
        Type(int type_tok, int arr_dim = 0)
            : type_tok(type_tok), arr_dim(arr_dim) {}

        Type(Type* t) : type_tok(t->type_tok), arr_dim(t->arr_dim) {}

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>) {
            return the_void_type();
        }

        virtual bool isFuncType() const { return false; }
        virtual bool isBool() const { return !this->isVoid(); }
        virtual bool isVoid() const;
        virtual bool isReal() const;
        virtual bool isIntegral() const;
        virtual bool isNumerical() const;
        virtual bool isEqType() const;
        virtual bool isOrdType() const;

        bool canSubstituteBy(Type) const;
        int getTypeTok() const { return type_tok; }
        int addArrDim() { return ++arr_dim; }
        int getArrDim() const { return arr_dim; }
        string toStr() const;
        virtual string typeExp() const;

        bool operator==(Type t2) const;
        bool operator!=(Type t) const { return !((*this) == t); }
};

class BoolType : public Type {
    public:
        BoolType();
        BoolType(BoolType*);

        bool isBool() const { return true; }
        bool isVoid() const { return false; }
        bool isReal() const { return false; }
        bool isIntegral() const { return true; }
        bool isNumerical() const { return true; }
        bool isEqType() const { return true; }
        bool isOrdType() const { return true; }
};

class FuncType : public Type {
    friend class FuncCallExp;
    friend class Env;
    typedef pair<shared_ptr<TypeVec>, shared_ptr<Type> > FuncTypeVariation;

    private:
        vector<FuncTypeVariation> variations;

    public:
        FuncType(TypeVec*, Type*);
        FuncType(FuncType*);

        bool isFuncType() const { return true; }
        bool isVoid() const { return false; }
        bool isReal() const { return false; }
        bool isIntegral() const { return false; }
        bool isNumerical() const { return false; }
        bool isEqType() const { return false; }
        bool isOrdType() const { return false; }

        string toStr() const;
        string typeExp(unsigned int) const;
        string typeExp() const {
            return this->typeExp(0);
        }

        bool operator==(const Type& t) const;
        bool operator!=(const Type& t) const {
            return !(*this == t);
        }
};

template <typename T>
class Vec : public AstNode {
    public:
        vector<unique_ptr<T> > items;

        void add(T* item) { items.push_back(unique_ptr<T>(item)); }
        unsigned int size() const { return items.size(); }

        shared_ptr<Type> typeCheck(Env* env, shared_ptr<Type>) { return the_void_type(); }

        string toStr() const {
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

class Scope {
    friend class Env;

    private:
        map<string, shared_ptr<Type> > sym_table;
        // if there's a missing symbol, all lookups in this scope should fail
        bool missing_symbol;

    public:
        Scope() : missing_symbol(false) {}
};

class Env {
    friend class EnvScopeGuard;

    private:
        vector<Scope> scopes;

        void pushScope() {
            Scope scope;
            scopes.push_back(scope);
            LOG("push scope (" << (scopes.size() - 1) << ')');
        }

        void popScope() {
            scopes.pop_back();
            LOG("pop scope");
        }

    public:
        shared_ptr<Type> findSymbolType(const string& ident)
            throw(DirtyScopeExn, SymbolNotFoundExn);
        void addSymbol(const std::string& ident, shared_ptr<Type> type)
            throw(SymbolRedeclExn);
};

class EnvScopeGuard {
    private:
        Env* env;

    public:
        EnvScopeGuard(Env* _env) : env(_env) { _env->pushScope(); }
        ~EnvScopeGuard() { env->popScope(); }
};

class Arg : public AstNode {
    friend class FuncDecl;

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
        IdentExp(string* id) : id(unique_ptr<string>(id)) {}
        string getIdentStr() const { return *id; }
        string toStr() const { return *id; }
        shared_ptr<Type> typeCheck(Env* env, shared_ptr<Type>) {
            return env->findSymbolType(*id);
        }
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
            string s = "NUMINT";
            // TODO: convert
            //ostringstream ss(s);
            //ss << val;
            return s;
        }

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
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

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
};

class StringLiteral : public Exp {
    private:
        unique_ptr<string> val;

    public:
        StringLiteral(string* s) : val(unique_ptr<string>(s)) {}
        string toStr() const { return *val; }
        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
};

class FuncCallExp : public Exp {
    friend class FuncCallArityMismatchExn;
    friend class NoMatchingFuncCall;

    private:
        unique_ptr<IdentExp> func_ident;
        unique_ptr<ExpVec> arg_exps;

    public:
        FuncCallExp(IdentExp* func_ident, ExpVec* arg_exps)
            : func_ident(unique_ptr<IdentExp>(func_ident)),
            arg_exps(unique_ptr<ExpVec>(arg_exps)) {}

        FuncCallExp(string* func_ident, ExpVec* arg_exps)
            : FuncCallExp(new IdentExp(func_ident), arg_exps) {}

        string toStr() const {
            return "(" + func_ident->toStr() + " " + arg_exps->toStr() + ")";
        }

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
};

class BinaryExp : public Exp {
    protected:
        unique_ptr<Exp> exp1;
        unique_ptr<Exp> exp2;

    public:
        BinaryExp(Exp* exp1, Exp* exp2)
            : exp1(unique_ptr<Exp>(exp1)), exp2(unique_ptr<Exp>(exp2)) {}

        virtual string operatorStr() const = 0;

        string toStr() const {
            return "(" + operatorStr() + " " + exp1->toStr() + " " + exp2->toStr() + ")";
        }
};

class NumericalBinaryExp : public BinaryExp {
    protected:
        virtual shared_ptr<Type> typeCheck(
                    shared_ptr<Type> t1,
                    shared_ptr<Type> t2,
                    Env* env) {
            if (t1->isReal()) {
                return t1;
            }
            if (t2->isReal()) {
                return t2;
            }
            return t1;
        }

    public:
        NumericalBinaryExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {}
        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
};

class UnaryExp : public Exp {
    protected:
        unique_ptr<Exp> exp;

    public:
        UnaryExp(Exp* exp) : exp(unique_ptr<Exp>(exp)) {}
};

class NewExp : public Exp {
    private:
        unique_ptr<Type> type;
        unique_ptr<Exp> exp;

    public:
        NewExp(Type* type, Exp* exp)
            : type(unique_ptr<Type>(type)), exp(unique_ptr<Exp>(exp)) {}

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

        string toStr() const {
            return "(new " + type->toStr() + " " + exp->toStr() + ")";
        }
};

class MinusExp : public UnaryExp {
    public:
        MinusExp(Exp* exp) : UnaryExp(exp) {}
        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
        string toStr() const { return "-" + exp->toStr(); }
};

class SumExp : public NumericalBinaryExp {
    public:
        SumExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "+"; }
};

class SubExp : public NumericalBinaryExp {
    public:
        SubExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "-"; }
};

class MultExp : public NumericalBinaryExp {
    public:
        MultExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "*"; }
};

class DivExp : public NumericalBinaryExp {
    public:
        DivExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "/"; }
};

class ComparisonBinaryExp : public BinaryExp {
    protected:
        void typeCheckOrderedComparison(
                shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            if (!t1->isOrdType() || !t2->isOrdType()) {
                SemanticExn e("invalid operand in comparison expression", this->lineno);
                e.emitError();
                throw e;
            }
        }

        void typeCheckEqualityComparison(
                shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            if (!t1->isEqType() || !t2->isEqType()) {
                SemanticExn e("invalid operand in comparison expression", this->lineno);
                e.emitError();
                throw e;
            }
        }

        virtual shared_ptr<Type> typeCheck(shared_ptr<Type>, shared_ptr<Type>, Env*) = 0;

    public:
        ComparisonBinaryExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {}

        shared_ptr<Type> typeCheck(Env* env, shared_ptr<Type>) {
            LOG("typechecking binary comparison expression (<, <=, >, >=, ==)");
            auto t1 = exp1->typeCheck(env, the_void_type());
            auto t2 = exp2->typeCheck(env, the_void_type());
            return typeCheck(t1, t2, env);
        }
};

class EqExp : public ComparisonBinaryExp {
    protected:
        shared_ptr<Type> typeCheck(shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            typeCheckEqualityComparison(t1, t2, env);
            return shared_ptr<Type>(new BoolType());
        }

    public:
        EqExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "=="; }
};

class GreaterEqExp : public ComparisonBinaryExp {
    protected:
        shared_ptr<Type> typeCheck(shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            typeCheckOrderedComparison(t1, t2, env);
            typeCheckEqualityComparison(t1, t2, env);
            return shared_ptr<Type>(new BoolType());
        }

    public:
        GreaterEqExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {}
        string operatorStr() const { return ">="; }
};

class LowerExp : public ComparisonBinaryExp {
    protected:
        shared_ptr<Type> typeCheck(shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            typeCheckOrderedComparison(t1, t2, env);
            return shared_ptr<Type>(new BoolType());
        }

    public:
        LowerExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "<"; }
};

class GreaterExp : public ComparisonBinaryExp {
    protected:
        shared_ptr<Type> typeCheck(shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            typeCheckOrderedComparison(t1, t2, env);
            return shared_ptr<Type>(new BoolType());
        }

    public:
        GreaterExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {}
        string operatorStr() const { return ">"; }
};

class LowerEqExp : public ComparisonBinaryExp {
    protected:
        shared_ptr<Type> typeCheck(shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            typeCheckOrderedComparison(t1, t2, env);
            typeCheckEqualityComparison(t1, t2, env);
            return shared_ptr<Type>(new BoolType());
        }

    public:
        LowerEqExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "<="; }
};

class NotExp : public UnaryExp {
    public:
        NotExp(Exp* exp) : UnaryExp(exp) {}
        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
        string toStr() const { return "!" + exp->toStr(); }
};

class BoolBinaryExp : public BinaryExp {
    public:
        BoolBinaryExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {}
        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
};

class AndExp : public BoolBinaryExp {
    public:
        AndExp(Exp* exp1, Exp* exp2) : BoolBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "&&"; }
};

class OrExp : public BoolBinaryExp {
    public:
        OrExp(Exp* exp1, Exp* exp2) : BoolBinaryExp(exp1, exp2) {}
        string operatorStr() const { return "||"; }
};

class Var : public Exp {
    private:
        unique_ptr<IdentExp> ident_exp;
        Vec<Exp> arr_subscripts;

    public:
        Var(string* ident)
            : ident_exp(unique_ptr<IdentExp>(new IdentExp(ident))) {}

        Var* push_subscript(Exp* subscript) {
            arr_subscripts.add(subscript);
            return this;
        }

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

        string toStr() const {
            if (arr_subscripts.size()) {
                return "(" + ident_exp->toStr() + " " + arr_subscripts.toStr() + ")";
            }
            return ident_exp->toStr();
        }
};

class Decl : public AstNode {
};

class VarDecl : public Decl {
    friend Block;

    private:
        unique_ptr<Type> type;
        unique_ptr<IdVec> idents;

    public:
        VarDecl(Type* type, IdVec* idents)
            : type(unique_ptr<Type>(type)), idents(unique_ptr<IdVec>(idents)) {}

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

        string toStr() const {
            string s = "(var " + type->toStr();
            for (auto it = idents->items.begin(); it != idents->items.end(); it++) {
                s += " " + **it;
            }
            return s + ")";
        }
};

class VarDeclVec : public Vec<VarDecl> {
    public:
        shared_ptr<Type> typeCheck(Env* env, shared_ptr<Type> et) {
            for (auto it = items.begin(); it != items.end(); it++) {
                (*it)->typeCheck(env, et);
            }
            return the_void_type();
        }
};

class Block : public AstNode {
    private:
        unique_ptr<VarDeclVec> vars;
        unique_ptr<CommandVec> commands;

    public:
        Block(VarDeclVec* var_decls, CommandVec* commands)
            : vars(unique_ptr<VarDeclVec>(var_decls)),
            commands(unique_ptr<CommandVec>(commands)) {}

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

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
        BlockStmt(Block* block) : block(unique_ptr<Block>(block)) {}
        shared_ptr<Type> typeCheck(Env* env, shared_ptr<Type> expected_type) {
            return block->typeCheck(env, expected_type);
        }
        string toStr() const { return block->toStr(); }
};

class Command : public AstNode {
    private:
        unique_ptr<Stmt> stmt;

    public:
        Command(Stmt* stmt) : stmt(unique_ptr<Stmt>(stmt)) {}
        Command(Block* block)
            : stmt(unique_ptr<BlockStmt>(new BlockStmt(block))) {}
        shared_ptr<Type> typeCheck(Env* env, shared_ptr<Type> expected_type) {
            return stmt->typeCheck(env, expected_type);
        }
        string toStr() const { return stmt->toStr(); }
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

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

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

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

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

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

        string toStr() const {
            return "(set " + var->toStr() + " " + rvalue->toStr() + ")";
        }
};

class ReturnStmt : public Stmt {
    private:
        unique_ptr<Exp> exp;

    public:
        ReturnStmt(Exp* exp = NULL) : exp(unique_ptr<Exp>(exp)) {}

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

        string toStr() const {
            if (exp == NULL) {
                return "ret";
            }
            return "(ret " + exp->toStr() + ")";
        }
};

class FuncCallStmt : public Stmt {
    private:
        unique_ptr<FuncCallExp> exp;

    public:
        FuncCallStmt(FuncCallExp* exp)
            : exp(unique_ptr<FuncCallExp>(exp)) {}
        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
        string toStr() const { return exp->toStr(); }
};

class FuncDecl : public Decl {
    private:
        unique_ptr<Type> ret_type;
        unique_ptr<string> id;
        shared_ptr<ArgsVec> args;
        unique_ptr<Block> block;

    public:
        FuncDecl(Type* ret_type, string* id, ArgsVec* args, Block* block)
            : ret_type(unique_ptr<Type>(ret_type)), id(unique_ptr<string>(id)),
            args(shared_ptr<ArgsVec>(args)), block(unique_ptr<Block>(block)) {}

        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);

        string toStr() const {
            return "(fun " + ret_type->toStr() + " " + *id + "\n" + args->toStr()
                + "\n" + monga::toStr(*block) + ")";
        }
};

class Prog : public Vec<Decl> {
    public:
        shared_ptr<Type> typeCheck(Env*, shared_ptr<Type>);
};

}; // namespace monga

#endif // MONGA_AST_
