#include <memory>
#include <vector>
#include <string>
#include <cstdio>
#include "ast.h"
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
    env->pushScope();
    this->vars->typeCheck(env);

    shared_ptr<Type> block_type(new Type(VOID));
    for (auto it = commands->items.begin(); it != commands->items.end(); it++) {
        auto t = (*it)->typeCheck(env);
        if (!t->isVoid()) {
            // TODO: check compatibility (avoid blocks having two return types)
            block_type = t;
        }
    }

    env->popScope();

    LOG("block returns \"" << block_type->typeExp() << '"');
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
    env->pushScope();
    for (auto it = args->items.begin(); it != args->items.end(); it++) {
        auto t = shared_ptr<Type>(new Type((*it)->type.get()));
        env->addSymbol(*(*it)->id, t);
    }
    auto block_type = block->typeCheck(env);
    if (!ret_type->canSubstituteBy(block_type)) {
        // TODO: error
    }
    env->popScope();

    return func_type;
}

}; // namespace monga
