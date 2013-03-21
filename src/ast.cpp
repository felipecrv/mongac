#include <memory>
#include <vector>
#include <string>
#include <cstdio>
#include "ast.h"
#include "exception.h"
using namespace monga;
#include "parser.h"

namespace monga {
using namespace std;

string toStr(const AstNode& node) { return node.toStr(); }
string toStr(const string& s) { return s; }

shared_ptr<Type> resolve_return_type(shared_ptr<Type> t1, shared_ptr<Type> t2) {
    if (t1->isVoid()) {
        return t2;
    }
    if (t2->isVoid()) {
        return t1;
    }
    if (t1 != t2) {
        // TODO: error
    }
    return t1;
}

shared_ptr<Type> the_void_type() {
    static shared_ptr<Type> void_t_ptr(new Type(VOID));
    return void_t_ptr;
}

shared_ptr<Type> AstNode::typeCheck(Env* env) {
    return the_void_type();
}

bool Type::isVoid() const { return type_tok == VOID; }
bool Type::isReal() const { return type_tok == FLOAT && this->arr_dim == 0; }
bool Type::isIntegral() const {
    return (type_tok == INT || type_tok == CHAR) && this->arr_dim == 0;
}
bool Type::isNumerical() const {
    return
        (type_tok == INT || type_tok == CHAR || type_tok == FLOAT) &&
        this->arr_dim == 0;
}
bool Type::isEqType() const {
    return
        (type_tok == CHAR && this->arr_dim <= 1) || // string is eq type
        (type_tok == INT && this->arr_dim == 0);
}
bool Type::isOrdType() const {
    return
        (type_tok == CHAR && this->arr_dim <= 1) || // string is ord type
        ((type_tok == INT || type_tok == FLOAT) && this->arr_dim == 0);
}

// we need a type_tok that's not equal to VOID, INT, FLOAT, or CHAR
BoolType::BoolType() : Type(IF) {}
BoolType::BoolType(BoolType* t) : Type(t) {}
FuncType::FuncType(TypeVec* arg_types, Type* ret_type)
    : Type(RETURN),
    arg_types(shared_ptr<TypeVec>(arg_types)),
    ret_type(shared_ptr<Type>(ret_type)) {}
FuncType::FuncType(FuncType* t)
    : Type(t), arg_types(t->arg_types), ret_type(t->ret_type) {}

bool Type::canSubstituteBy(shared_ptr<Type> replacement) const {
    if (this->getArrDim() != replacement->getArrDim()) {
        return false;
    }
    if (this->isIntegral() && replacement->isReal()) {
        return false;
    }
    // TODO: implement this
    return true;
}

string Type::toStr() const {
    string type;
    switch (type_tok) {
        case INT:
            type = "int";
            break;
        case FLOAT:
            type = "float";
            break;
        case CHAR:
            type = "char";
            break;
        case VOID:
            type = "void";
            break;
        case IF:
            type = "bool";
            break;
        case RETURN: // FuncType overrides typeExp()
            type = "<FUNC>";
            break;
        default:
            type = "<TYPE>";
            break;
    }

    char dim[20];
    sprintf(dim, "%d", arr_dim);
    return "(t " + type + " " + string(dim) + ")";
}

string Type::typeExp() const {
    string type;
    switch (type_tok) {
        case INT:
            type = "int";
            break;
        case FLOAT:
            type = "float";
            break;
        case CHAR:
            type = "char";
            break;
        case VOID:
            type = "void";
            break;
        case IF:
            type = "bool";
            break;
        case RETURN: // FuncType overrides typeExp()
            type = "FUNC";
            break;
        default:
            type = "TYPE";
            break;
    }
    for (int i = 0; i < arr_dim; i++) {
        type += "[]";
    }
    return type;
}

string FuncType::toStr() const {
    string s = "(t " + arg_types->toStr() + " " + ret_type->toStr() + ")";
    return s;
}

string FuncType::typeExp() const {
    string type = "(";
    for (auto it = arg_types->items.begin(); it != arg_types->items.end();)  {
        type += (*it)->typeExp();
        if (++it != arg_types->items.end()) {
            type += ", ";
        }
    }
    type += ") -> " + ret_type->typeExp();
    return type;
}

bool Type::operator==(const Type& t) const {
    return this->type_tok == t.type_tok && this->arr_dim == t.arr_dim;
}

bool FuncType::operator==(const Type& t) const {
    if (t.isFuncType()) {
        unsigned int arity = this->arg_types->size();
        const FuncType& ft = (const FuncType&) t;
        bool equals =
            arity == ft.arg_types->size() && *this->ret_type == *ft.ret_type;
        for (unsigned int i = 0; equals && i < arity; i++) {
            equals = *this->arg_types->items[i] == *ft.arg_types->items[i];
        }
        return equals;
    }
    return false;
}

shared_ptr<Type> Env::findSymbolType(const string& ident)
        throw(MissingSymExn, SymbolNotFoundExn) {
    for (int i = ((int) scopes.size()) - 1; i >= 0; i--) {
        // we can't rely on this scope if a symbol is missing
        if (scopes[i].missing_symbol) {
            MissingSymExn e;
            e.emitError();
            throw e;
        }
        auto sym_pair = scopes[i].sym_table.find(ident);
        if (sym_pair != scopes[i].sym_table.end()) {
            // LOG("ENV: found '" << ident << ": " <<
            //     sym_pair->second->typeExp() << "' in scope " << i);
            return sym_pair->second;
        }
    }
    {
        SymbolNotFoundExn e(ident);
        e.emitError();
        throw e;
    }
}

void Env::addSymbol(const std::string& ident, shared_ptr<Type> type)
        throw(SymbolRedeclExn) {
    LOG("adding symbol '" << ident << ": " << type->typeExp() <<
            "' to current scope (" << (scopes.size() - 1) << ")");
    Scope& cur_scope = scopes[scopes.size() - 1];
    auto sym_pair = cur_scope.sym_table.find(ident);
    if (sym_pair != cur_scope.sym_table.end()) {
        cur_scope.missing_symbol = true;
        SymbolRedeclExn e(ident, sym_pair->second, type);
        e.emitError();
        throw e;
    }
    cur_scope.sym_table[ident] = type;
}

shared_ptr<Type> IntLiteral::typeCheck(Env* env) {
    LOG("typechecking int literal");
    return shared_ptr<Type>(new Type(INT));
}

shared_ptr<Type> FloatLiteral::typeCheck(Env* env) {
    LOG("typechecking float literal");
    return shared_ptr<Type>(new Type(FLOAT));
}

shared_ptr<Type> StringLiteral::typeCheck(Env* env) {
    LOG("typechecking string literal");
    return shared_ptr<Type>(new Type(CHAR, 1));
}

shared_ptr<Type> Block::typeCheck(Env* env) {
    EnvScopeGuard g(env);
    this->vars->typeCheck(env);

    shared_ptr<Type> block_type(new Type(VOID));
    for (auto it = commands->items.begin(); it != commands->items.end(); it++) {
        auto t = (*it)->typeCheck(env);
        if (!t->isVoid()) {
            // TODO: check compatibility (avoid blocks having two return types)
            block_type = t;
        }
    }

    LOG("block returns '" << block_type->typeExp() << '"');
    return block_type;
}

shared_ptr<Type> VarDecl::typeCheck(Env* env) {
    shared_ptr<Type> symbol_type(new Type(this->type.get()));
    for (auto ident_it = this->idents->items.begin();
            ident_it != this->idents->items.end();
            ident_it++) {
        env->addSymbol(**ident_it, symbol_type);
    }
    return the_void_type();
}

shared_ptr<Type> FuncDecl::typeCheck(Env* env) {
    LOG("typechecking function declaration");

    // build the function type
    TypeVec* arg_types = new TypeVec();
    for (auto it = args->items.begin(); it != args->items.end(); it++) {
        Type* t = new Type((*it)->type.get());
        arg_types->add(t);
    }
    Type* ret_t = new Type(ret_type.get());
    shared_ptr<FuncType> func_type(new FuncType(arg_types, ret_t));

    // add function to the current scope
    env->addSymbol(*id, func_type);

    // create a new scope, add the arguments to this scope and then type
    // check the function block
    {
        EnvScopeGuard g(env);
        for (auto it = args->items.begin(); it != args->items.end(); it++) {
            auto t = shared_ptr<Type>(new Type((*it)->type.get()));
            env->addSymbol(*(*it)->id, t);
        }
        try {
            auto block_type = block->typeCheck(env);
            if (!ret_type->canSubstituteBy(block_type)) {
                // TODO: error
            }
        } catch (SemanticExn& e) {
            // the block failed to typecheck for some reason. We ignore it to
            // carry on with the typechecking of other declarations
            if (!e.errorEmitted()) {
                throw e;
            }
        }
    }
    return func_type;
}

shared_ptr<Type> FuncCallExp::typeCheck(Env* env) {
    LOG("typechecking function call expression");
    auto symbol_type = env->findSymbolType(func_ident->getIdentStr());
    if (symbol_type->isFuncType()) {
        auto func_type = (FuncType*) symbol_type.get();
        if (func_type->arg_types->size() != arg_exps->size()) {
            FuncCallArityMismatchExn e;
            e.emitError();
            throw e;
        }

        auto type_it = func_type->arg_types->items.begin();
        auto exp_it = arg_exps->items.begin();
        for (unsigned int i = 1;
                type_it != func_type->arg_types->items.end();
                type_it++, exp_it++, i++) {
            auto exp_type = (*exp_it)->typeCheck(env);
            if (!(*type_it)->canSubstituteBy(exp_type)) {
                FuncCallTypeMismatchExn e(
                        func_ident->getIdentStr(),
                        new Type((*type_it).get()),
                        new Type(exp_type.get()),
                        i);
                e.emitError();
                throw e;
            }
        }

        return func_type->ret_type;
    } else {
        IdentifierNotAFuncExn e;
        e.emitError();
        throw e;
    }
}

shared_ptr<Type> NumericalBinaryExp::typeCheck(Env* env) {
    LOG("typechecking numerical binary expression (+, -, *, /)");
    auto t1 = exp1->typeCheck(env);
    auto t2 = exp2->typeCheck(env);
    if (!t1->isNumerical()) {
        NonNumericalOperandExn e;
        e.emitError();
        throw e;
    }
    if (!t2->isNumerical()) {
        NonNumericalOperandExn e;
        e.emitError();
        throw e;
    }
    return typeCheck(t1, t2, env);
}

shared_ptr<Type> NewExp::typeCheck(Env* env) {
    LOG("typechecking new expression");

    auto exp_type = exp->typeCheck(env);
    if (!exp_type->isIntegral()) {
        NonIntegralAllocationSizeExn e;
        e.emitError();
        throw e;
    }

    if (type->isVoid()) {
        InvalidOperandTypeExn e;
        e.emitError();
        throw e;
    }

    Type* t = new Type(type.get());
    t->addArrDim();
    return shared_ptr<Type>(t);
}

shared_ptr<Type> MinusExp::typeCheck(Env* env) {
    LOG("typechecking minus expression");
    auto t = exp->typeCheck(env);
    if (!t->isNumerical()) {
        NonNumericalOperandExn e;
        e.emitError();
        throw e;
    }
    return t;
}

shared_ptr<Type> NotExp::typeCheck(Env* env) {
    LOG("typechecking not expression");
    auto t = exp->typeCheck(env);
    if (!t->isBool()) {
        InvalidOperandTypeExn e;
        e.emitError();
        throw e;
    }
    return t;
}

shared_ptr<Type> BoolBinaryExp::typeCheck(Env* env) {
    LOG("typechecking bool binary expression (&&, ||)");
    auto t1 = exp1->typeCheck(env);
    auto t2 = exp2->typeCheck(env);
    if (!t1->isBool()) {
        InvalidOperandTypeExn e;
        e.emitError();
        throw e;
    }
    if (!t2->isBool()) {
        InvalidOperandTypeExn e;
        e.emitError();
        throw e;
    }
    return shared_ptr<Type>(new BoolType());
}

shared_ptr<Type> Var::typeCheck(Env* env) {
    LOG("typechecking var expression");
    // type of the identifier
    auto ident_type = ident_exp->typeCheck(env);

    // check if the []s can be applied...
    if (((int) arr_subscripts.size()) > ident_type->getArrDim()) {
        InvalidArrSubscriptExn e;
        e.emitError();
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
            e.emitError();
            throw e;
        }
    }

    return shared_ptr<Type>(var_type);
}

shared_ptr<Type> IfStmt::typeCheck(Env* env) {
    LOG("typechecking if statement");
    auto cond_type = cond_exp->typeCheck(env);
    if (!cond_type->isBool()) {
        NonBoolCondExn e;
        e.emitError();
        throw e;
    }
    if (!else_cmd) {
        return then_cmd->typeCheck(env);
    }
    auto then_cmd_type = then_cmd->typeCheck(env);
    auto else_cmd_type = else_cmd->typeCheck(env);
    return resolve_return_type(then_cmd_type, else_cmd_type);
}

shared_ptr<Type> WhileStmt::typeCheck(Env* env) {
    LOG("typechecking while statement");
    auto cond_type = cond_exp->typeCheck(env);
    if (!cond_type->isBool()) {
        NonBoolCondExn e;
        e.emitError();
        throw e;
    }
    return block->typeCheck(env);
}

shared_ptr<Type> AssignStmt::typeCheck(Env* env) {
    LOG("typechecking assignment");
    auto var_type = var->typeCheck(env);
    auto rvalue_type = rvalue->typeCheck(env);
    if (!var_type->canSubstituteBy(rvalue_type)) {
        InvalidAssignExn e(var_type, rvalue_type);
        e.emitError();
        throw e;
    }
    return the_void_type();
}

shared_ptr<Type> ReturnStmt::typeCheck(Env* env) {
    LOG("typechecking return statement");
    if (exp == NULL) {
        return the_void_type();
    }
    return exp->typeCheck(env);
}

shared_ptr<Type> FuncCallStmt::typeCheck(Env* env) {
    LOG("typechecking function call statement");
    exp->typeCheck(env);
    return the_void_type();
}

shared_ptr<Type> Prog::typeCheck(Env* env) {
    LOG("typechecking program");
    {
        EnvScopeGuard g(env);
        for (auto it = items.begin(); it != items.end(); it++) {
            (*it)->typeCheck(env);
        }
    }
    return the_void_type();
}

}; // namespace monga
