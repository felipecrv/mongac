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

#define LOG(os_ops)  std::cout << __FILE__ << ":" << __LINE__ << ": " << os_ops << std::endl

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

class AstNode {
    public:
        virtual string toStr() const = 0;
        virtual shared_ptr<Type> typeCheck(Env*);
};

string toStr(const AstNode& node);
string toStr(const string& s);
shared_ptr<Type> resolve_return_type(shared_ptr<Type>, shared_ptr<Type>);

class Type : public AstNode {
    friend class Var;

    private:
        int type_tok;
        int arr_dim;

    public:
        Type(int type_tok, int arr_dim = 0)
            : type_tok(type_tok), arr_dim(arr_dim) {}

        Type(Type* t) : type_tok(t->type_tok), arr_dim(t->arr_dim) {}

        virtual bool isFuncType() const { return false; }
        virtual bool isBool() const { return false; }
        virtual bool isVoid() const;
        virtual bool isReal() const;
        virtual bool isIntegral() const;
        virtual bool isNumerical() const;
        virtual bool isEqType() const;
        virtual bool isOrdType() const;

        bool canSubstituteBy(shared_ptr<Type> replacement) const;
        int addArrDim() { return ++arr_dim; }
        int getArrDim() const { return arr_dim; }
        string toStr() const;
        virtual string typeExp() const;
};

shared_ptr<Type> the_void_type();

class BoolType : public Type {
    public:
        BoolType();
        BoolType(BoolType*);

        bool isBool() const { return true; }
        bool isVoid() const { return false; }
        bool isReal() const { return false; }
        bool isIntegral() const { return false; }
        bool isNumerical() const { return false; }
        bool isEqType() const { return true; }
        bool isOrdType() const { return false; }
};

class FuncType : public Type {
    friend FuncCallExp;

    private:
        shared_ptr<TypeVec> arg_types;
        shared_ptr<Type> ret_type;

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
        string typeExp() const;
};

template <typename T>
class Vec : public AstNode {
    public:
        vector<unique_ptr<T> > items;

        void add(T* item) { items.push_back(unique_ptr<T>(item)); }
        unsigned int size() const { return items.size(); }

        // TODO: make this pure virtual
        shared_ptr<Type> typeCheck(Env* env) {
            return the_void_type();
        }

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

class Env {
    friend class EnvScopeGuard;

    typedef map<string, shared_ptr<Type> > Scope;

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
        shared_ptr<Type> findSymbolType(const string& ident) {
            for (int i = scopes.size() - 1; i >= 0; i--) {
                auto type = scopes[i].find(ident);
                if (type != scopes[i].end()) {
                    //LOG("ENV: found \"" << ident << ": " << type->second->typeExp() << "\" in scope " << i);
                    return type->second;
                }
            }
            {
                SymbolNotFoundExn e(ident);
                throw e;
            }
        }

        void addSymbol(const std::string& ident, shared_ptr<Type> type) {
            // TODO: check it's been declared before (not here)
            scopes[scopes.size() - 1][ident] = type;
            LOG("add symbol \"" << ident << ": " << type->typeExp() <<
                    "\" to current scope (" << (scopes.size() - 1) << ")");
        }
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
        IdentExp(string* id) : id(unique_ptr<string>(id)) {
        }

        string getIdentStr() const { return *id; }

        string toStr() const { return *id; }

        shared_ptr<Type> typeCheck(Env* env) { return env->findSymbolType(*id); }
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

        shared_ptr<Type> typeCheck(Env* env);
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

        shared_ptr<Type> typeCheck(Env* env);
};

class StringLiteral : public Exp {
    private:
        unique_ptr<string> val;

    public:
        StringLiteral(string* s) : val(unique_ptr<string>(s)) {
        }

        string toStr() const { return *val; }

        shared_ptr<Type> typeCheck(Env* env);
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

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking function call expression");
            auto symbol_type = env->findSymbolType(func_ident->getIdentStr());
            if (symbol_type->isFuncType()) {
                auto func_type = (FuncType*) symbol_type.get();
                if (func_type->arg_types->size() != arg_exps->size()) {
                    FuncCallArityMismatchExn e;
                    throw e;
                }

                auto type_it = func_type->arg_types->items.begin(); 
                auto exp_it = arg_exps->items.begin();
                for (; type_it != func_type->arg_types->items.end(); type_it++, exp_it++) {
                    auto exp_type = (*exp_it)->typeCheck(env);
                    if (!(*type_it)->canSubstituteBy(exp_type)) {
                        FuncCallTypeMismatchExn e;
                        throw e;
                    }
                }

                return func_type->ret_type;
            } else {
                IdentifierNotAFuncExn e;
                throw e;
            }
        }
};

class BinaryExp : public Exp {
    protected:
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
        NumericalBinaryExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking numerical binary expression (+, -, *, /)");
            auto t1 = exp1->typeCheck(env);
            auto t2 = exp2->typeCheck(env);
            if (!t1->isNumerical()) {
                NonNumericalOperandExn e;
                throw e;
            }
            if (!t2->isNumerical()) {
                NonNumericalOperandExn e;
                throw e;
            }
            return typeCheck(t1, t2, env);
        }
};

class UnaryExp : public Exp {
    protected:
        unique_ptr<Exp> exp;

    public:
        UnaryExp(Exp* exp) : exp(unique_ptr<Exp>(exp)) {
        }
};

class NewExp : public Exp {
    private:
        unique_ptr<Type> type;
        unique_ptr<Exp> exp;

    public:
        NewExp(Type* type, Exp* exp)
            : type(unique_ptr<Type>(type)), exp(unique_ptr<Exp>(exp)) {
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking new expression");

            auto exp_type = exp->typeCheck(env);
            if (!exp_type->isIntegral()) {
                NonIntegralAllocationSizeExn e;
                throw e;
            }

            if (type->isVoid()) {
                InvalidOperandTypeExn e;
                throw e;
            }

            Type* t = new Type(type.get());
            t->addArrDim();
            return shared_ptr<Type>(t);
        }

        string toStr() const {
            return "(new " + type->toStr() + " " + exp->toStr() + ")";
        }
};

class MinusExp : public UnaryExp {
    public:
        MinusExp(Exp* exp) : UnaryExp(exp) {
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking minus expression");
            auto t = exp->typeCheck(env);
            if (!t->isNumerical()) {
                NonNumericalOperandExn e;
                throw e;
            }
            return t;
        }

        string toStr() const { return "-" + exp->toStr(); }
};

class SumExp : public NumericalBinaryExp {
    public:
        SumExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "+"; }
};

class SubExp : public NumericalBinaryExp {
    public:
        SubExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "-"; }
};

class MultExp : public NumericalBinaryExp {
    public:
        MultExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "*"; }
};

class DivExp : public NumericalBinaryExp {
    public:
        DivExp(Exp* exp1, Exp* exp2) : NumericalBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "/"; }
};

class ComparisonBinaryExp : public BinaryExp {
    protected:
        void typeCheckOrderedComparison(
                shared_ptr<Type> t1,
                shared_ptr<Type> t2,
                Env* env) {
            if (!t1->isOrdType()) {
                NonOrdTypeComparisonExn e;
                throw e;
            }
            if (!t2->isOrdType()) {
                NonOrdTypeComparisonExn e;
                throw e;
            }
        }

        void typeCheckEqualityComparison(
                shared_ptr<Type> t1,
                shared_ptr<Type> t2,
                Env* env) {
            if (!t1->isEqType()) {
                NonEqTypeComparisonExn e;
                throw e;
            }
            if (!t2->isEqType()) {
                NonEqTypeComparisonExn e;
                throw e;
            }
        }

        virtual shared_ptr<Type> typeCheck(shared_ptr<Type>, shared_ptr<Type>, Env*) = 0;

    public:
        ComparisonBinaryExp(Exp* exp1, Exp* exp2) : BinaryExp(exp1, exp2) {
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking binary comparison expression (<, <=, >, >=, ==)");
            auto t1 = exp1->typeCheck(env);
            auto t2 = exp2->typeCheck(env);
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
        EqExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {
        }

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
        GreaterEqExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return ">="; }
};

class LowerExp : public ComparisonBinaryExp {
    protected:
        shared_ptr<Type> typeCheck(shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            typeCheckOrderedComparison(t1, t2, env);
            return shared_ptr<Type>(new BoolType());
        }

    public:
        LowerExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "<"; }
};

class GreaterExp : public ComparisonBinaryExp {
    protected:
        shared_ptr<Type> typeCheck(shared_ptr<Type> t1, shared_ptr<Type> t2, Env* env) {
            typeCheckOrderedComparison(t1, t2, env);
            return shared_ptr<Type>(new BoolType());
        }

    public:
        GreaterExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {
        }

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
        LowerEqExp(Exp* exp1, Exp* exp2) : ComparisonBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "<="; }
};

class NotExp : public UnaryExp {
    public:
        NotExp(Exp* exp) : UnaryExp(exp) {
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking not expression");
            auto t = exp->typeCheck(env);
            if (!t->isBool()) {
                InvalidOperandTypeExn e;
                throw e;
            }
            return t;
        }

        string toStr() const { return "!" + exp->toStr(); }
};

class BoolBinaryExp : public BinaryExp {
    public:
        BoolBinaryExp(Exp* exp1, Exp* exp2)
            : BinaryExp(exp1, exp2) {
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking bool binary expression (&&, ||)");
            auto t1 = exp1->typeCheck(env);
            auto t2 = exp2->typeCheck(env);
            if (!t1->isBool()) {
                InvalidOperandTypeExn e;
                throw e;
            }
            if (!t2->isBool()) {
                InvalidOperandTypeExn e;
                throw e;
            }
            return shared_ptr<Type>(new BoolType());
        }
};

class AndExp : public BoolBinaryExp {
    public:
        AndExp(Exp* exp1, Exp* exp2) : BoolBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "&&"; }
};

class OrExp : public BoolBinaryExp {
    public:
        OrExp(Exp* exp1, Exp* exp2) : BoolBinaryExp(exp1, exp2) {
        }

        string operatorStr() const { return "||"; }
};

class Var : public Exp {
    private:
        unique_ptr<IdentExp> ident_exp;
        Vec<Exp> arr_subscripts;

    public:
        Var(string* ident) : ident_exp(unique_ptr<IdentExp>(new IdentExp(ident))) {
        }

        Var* push_subscript(Exp* subscript) {
            arr_subscripts.add(subscript);
            return this;
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking var expression");
            // type of the identifier
            auto ident_type = ident_exp->typeCheck(env);

            // check if the []s can be applied...
            if (((int) arr_subscripts.size()) > ident_type->getArrDim()) {
                InvalidArrSubscriptExn e;
                throw e;
            }
            // ...and calculates the lvalue (var expression) type
            auto var_type = new Type(ident_type.get());
            var_type->arr_dim = ident_type->getArrDim() - arr_subscripts.size();

            // check whether all subscripts are ints
            for (auto it = arr_subscripts.items.begin(); it != arr_subscripts.items.end(); it++) {
                LOG("SUB: " << (*it)->typeCheck(env)->typeExp());
                if (!(*it)->typeCheck(env)->isIntegral()) {
                    InvalidArrSubscriptExn e;
                    throw e;
                }
            }

            return shared_ptr<Type>(var_type);
        }

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
            : type(unique_ptr<Type>(type)), idents(unique_ptr<IdVec>(idents)) {
        }

        shared_ptr<Type> typeCheck(Env* env);

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
        shared_ptr<Type> typeCheck(Env* env) {
            for (auto it = items.begin(); it != items.end(); it++) {
                (*it)->typeCheck(env);
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
            commands(unique_ptr<CommandVec>(commands)) {
        }

        shared_ptr<Type> typeCheck(Env*);

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
        
        shared_ptr<Type> typeCheck(Env* env) {
            return block->typeCheck(env);
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

        shared_ptr<Type> typeCheck(Env* env) {
            return stmt->typeCheck(env);
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

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking if statement");
            auto cond_type = cond_exp->typeCheck(env);
            if (!cond_type->isBool()) {
                NonBoolCondExn e;
                throw e;
            }
            if (!else_cmd) {
                return then_cmd->typeCheck(env);
            }
            auto then_cmd_type = then_cmd->typeCheck(env);
            auto else_cmd_type = else_cmd->typeCheck(env);
            return resolve_return_type(then_cmd_type, else_cmd_type);
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
        
        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking while statement");
            auto cond_type = cond_exp->typeCheck(env);
            if (!cond_type->isBool()) {
                NonBoolCondExn e;
                throw e;
            }
            return block->typeCheck(env);
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

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking assignment");
            auto var_type = var->typeCheck(env);
            auto rvalue_type = rvalue->typeCheck(env);
            if (!var_type->canSubstituteBy(rvalue_type)) {
                InvalidAssignExn e(var_type, rvalue_type);
                throw e;
            }
            return the_void_type();
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

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking return statement");
            if (exp == NULL) {
                return the_void_type();
            }
            return exp->typeCheck(env);
        }

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
            : exp(unique_ptr<FuncCallExp>(exp)) {
        }

        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking function call statement");
            exp->typeCheck(env);
            return the_void_type();
        }

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
            args(shared_ptr<ArgsVec>(args)), block(unique_ptr<Block>(block)) {
        }

        shared_ptr<Type> typeCheck(Env*);

        string toStr() const {
            return "(fun " + ret_type->toStr() + " " + *id + "\n" + args->toStr()
                + "\n" + monga::toStr(*block) + ")";
        }
};

class Prog : public Vec<Decl> {
    public:
        shared_ptr<Type> typeCheck(Env* env) {
            LOG("typechecking program");
            EnvScopeGuard g(env);
            for (auto it = items.begin(); it != items.end(); it++) {
                (*it)->typeCheck(env);
            }
            return the_void_type();
        }
};

}; // namespace monga

#endif // MONGA_AST_
