#include <memory>
#include <vector>
#include <string>
#include <cstdio>
#include <utility>
#include "ast.h"
#include "exception.h"
using namespace monga;
#include "parser.h"

namespace monga {
using namespace std;

string toStr(const AstNode& node) { return node.toStr(); }
string toStr(const string& s) { return s; }

shared_ptr<Type> the_void_type() {
    static shared_ptr<Type> void_t_ptr(new Type(VOID));
    return void_t_ptr;
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
        ((type_tok == INT || type_tok == FLOAT) && this->arr_dim == 0);
}
bool Type::isOrdType() const {
    return
        (type_tok == CHAR && this->arr_dim <= 1) || // string is ord type
        ((type_tok == INT || type_tok == FLOAT) && this->arr_dim == 0);
}

// we use INT as type_tok for booleans
BoolType::BoolType() : Type(INT) {}
BoolType::BoolType(BoolType* t) : Type(t) {}
FuncType::FuncType(TypeVec* arg_types, Type* ret_type)
        : Type(RETURN) {
    variations.push_back(make_pair(
           shared_ptr<TypeVec>(arg_types),
           shared_ptr<Type>(ret_type)));
}

FuncType::FuncType(FuncType* t)
    : Type(t), variations(t->variations) {}

bool Type::canSubstituteBy(Type replacement) const {
    // if arrays of any dimension, the scalar types should totally match for a
    // substitution to be possible
    if ((*this == replacement) ||
            (this->isIntegral() && replacement.isIntegral()) ||
            (this->isReal() && replacement.isIntegral()) ||
            (this->isReal() && replacement.isReal())) {
        return true;
    }
    // different array dimensions... and all other cases
    return false;
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
    // TODO: serialize all variations
    string s = "(t " + variations[0].first->toStr() +
        " " + variations[0].second->toStr() + ")";
    return s;
}

string FuncType::typeExp(unsigned int ith_vartn) const {
    auto arg_types = variations[ith_vartn].first;
    auto ret_type = variations[ith_vartn].second;
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

bool Type::operator==(Type t2) const {
    return this->type_tok == t2.type_tok && this->arr_dim == t2.arr_dim;
}

// we check if the first variation of the function type t is in this' variations
bool FuncType::operator==(const Type& t) const {
    if (t.isFuncType()) {
        const FuncType& ft = (const FuncType&) t;
        auto ft_arg_types = ft.variations[0].first;
        auto ft_ret_type = ft.variations[0].second;

        for (auto it = variations.begin(); it != variations.end(); it++) {
            auto arg_types = it->first;
            auto ret_type = it->second;
            unsigned int arity = arg_types->size();

            bool equals =
                arity == ft_arg_types->size() && *ret_type == *ft_ret_type;
            for (unsigned int i = 0; equals && i < arity; i++) {
                equals = *arg_types->items[i] == *ft_arg_types->items[i];
            }
            if (equals) {
                return true;
            }
        }
    }
    return false;
}

shared_ptr<Type> Env::findSymbolType(const string& ident)
        throw(DirtyScopeExn, SymbolNotFoundExn) {
    for (int i = ((int) scopes.size()) - 1; i >= 0; i--) {
        // we can't rely on this scope if a symbol is missing
        if (scopes[i].missing_symbol) {
            DirtyScopeExn e;
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
        if (sym_pair->second->isFuncType()) {
            FuncType* func_type_from_sym = (FuncType*) sym_pair->second.get();
            FuncType* func_type = (FuncType*) type.get();
            if (*func_type_from_sym != *func_type) {
                func_type_from_sym->variations.push_back(func_type->variations[0]);
                return;
            }
        }
        cur_scope.missing_symbol = true;
        SymbolRedeclExn e(ident, sym_pair->second, type);
        throw e;
    }
    cur_scope.sym_table[ident] = type;
}

shared_ptr<Type> IntLiteral::typeCheck(Env* env, shared_ptr<Type>) {
    LOG("typechecking int literal");
    return shared_ptr<Type>(new Type(INT));
}

shared_ptr<Type> FloatLiteral::typeCheck(Env* env, shared_ptr<Type>) {
    LOG("typechecking float literal");
    return shared_ptr<Type>(new Type(FLOAT));
}

shared_ptr<Type> StringLiteral::typeCheck(Env* env, shared_ptr<Type>) {
    LOG("typechecking string literal");
    return shared_ptr<Type>(new Type(CHAR, 1));
}

shared_ptr<Type> Block::typeCheck(Env* env, shared_ptr<Type> expected_type) {
    EnvScopeGuard g(env);
    this->vars->typeCheck(env, expected_type);

    shared_ptr<Type> block_type(new Type(VOID));
    for (auto it = commands->items.begin(); it != commands->items.end(); it++) {
        auto t = (*it)->typeCheck(env, expected_type);
        if (!t->isVoid()) {
            block_type = t;
            if (!expected_type->canSubstituteBy(*block_type)) {
                ReturnTypeMismatchExn e(*expected_type, *block_type, (*it)->lineno);
                e.emitError();
                throw e;
            }
        }
    }

    LOG("block returns '" << block_type->typeExp() << '"');
    return block_type;
}

shared_ptr<Type> VarDecl::typeCheck(Env* env, shared_ptr<Type> et) {
    shared_ptr<Type> symbol_type(new Type(this->type.get()));
    try {
        for (auto ident_it = this->idents->items.begin();
                ident_it != this->idents->items.end();
                ident_it++) {
            env->addSymbol(**ident_it, symbol_type);
        }
    } catch (SymbolRedeclExn& e) {
        // TODO: would be cool to have each name's lineno
        e.lineno = this->lineno;
        e.emitError();
        throw e;
    }
    return the_void_type();
}

shared_ptr<Type> FuncDecl::typeCheck(Env* env, shared_ptr<Type> et) {
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
    try {
        env->addSymbol(*id, func_type);
    } catch (SymbolRedeclExn& e) {
        // TODO: would be cool to have the id's lineno
        e.lineno = this->lineno;
        e.emitError();
        throw e;
    }

    // create a new scope, add the arguments to this scope and then type
    // check the function block
    {
        EnvScopeGuard g(env);
        try {
            for (auto it = args->items.begin(); it != args->items.end(); it++) {
                auto t = shared_ptr<Type>(new Type((*it)->type.get()));
                env->addSymbol(*(*it)->id, t);
            }
        } catch (SymbolRedeclExn& e) {
            // TODO: would be cool to have the argument id lineno
            e.lineno = this->lineno;
            e.emitError();
            throw e;
        }
        try {
            auto block_type = block->typeCheck(env, /* expected= */shared_ptr<Type>(new Type(ret_t)));
            if (!ret_type->canSubstituteBy(*block_type)) {
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

shared_ptr<Type> FuncCallExp::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking function call expression");
    shared_ptr<Type> symbol_type;
    try {
        symbol_type = env->findSymbolType(func_ident->getIdentStr());
    } catch (SymbolNotFoundExn& e) {
        e.lineno = this->lineno;
        e.emitError();
        throw e;
    }
    if (symbol_type->isFuncType()) {
        auto func_type = (FuncType*) symbol_type.get();

        // try to pick which variation of the function will be called
        unsigned int matched = 0;
        shared_ptr<Type> ret_type;

        // type checks the expressions passed to the function
        vector<shared_ptr<Type> > exp_types;
        for (auto it = arg_exps->items.begin();
                it != arg_exps->items.end();
                it++) {
            auto exp_type = (*it)->typeCheck(env, et);
            exp_types.push_back(exp_type);
        }

        // match against all possible variations
        for (auto it = func_type->variations.begin();
                it != func_type->variations.end();
                it++) {
            auto arg_types = it->first;
            ret_type = it->second;

            // checks arity for this variation
            if (arg_types->size() != arg_exps->size()) {
                // if there's a single variation we fail with a more specific
                // message
                if (func_type->variations.size() == 1) {
                    FuncCallArityMismatchExn e(arg_types->size(), this);
                    e.emitError();
                    throw e;
                }
                continue;
            }

            unsigned int i = 1;
            auto arg_type_it = arg_types->items.begin();
            auto exp_it = arg_exps->items.begin();
            auto exp_type_it = exp_types.begin();
            for (; arg_type_it != arg_types->items.end();
                    i++, arg_type_it++, exp_it++, exp_type_it++) {
                if (!(*arg_type_it)->canSubstituteBy(**exp_type_it)) {
                    // if there's a single variation we fail with a more specific
                    // message
                    if (func_type->variations.size() == 1) {
                        FuncCallTypeMismatchExn e(
                                func_ident->getIdentStr(),
                                new Type((*arg_type_it).get()),
                                new Type(**exp_type_it),
                                i,
                                (*exp_it)->lineno);
                        e.emitError();
                        throw e;
                    }
                    goto next_variation;
                }
            }
            ++matched;
next_variation:
            continue;
        }

        if (!matched) {
            string exp_types_as_str = "";
            bool first = true;
            for(auto exp_type_it = exp_types.begin();
                    exp_type_it != exp_types.end();
                    exp_type_it++) {
                if (!first) {
                    exp_types_as_str += ", ";
                } else {
                    first = false;
                }
                exp_types_as_str += (*exp_type_it)->typeExp();
            }
            NoMatchingFuncCall e(this, exp_types_as_str);
            e.emitError();
            throw e;
        }
        if (matched > 1) {
            SemanticExn e("ambiguous function call to '" +
                    func_ident->getIdentStr() +
                    "'. " + to_string(matched) + " matches found",
                    this->lineno);
            e.emitError();
            throw e;
        }
        return ret_type;
    } else {
        IdentifierNotAFuncExn e(func_ident->getIdentStr(), symbol_type, this->lineno);
        e.emitError();
        throw e;
    }
}

shared_ptr<Type> NumericalBinaryExp::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking numerical binary expression (+, -, *, /)");
    auto t1 = exp1->typeCheck(env, et);
    auto t2 = exp2->typeCheck(env, et);
    if (!t1->isNumerical() || !t2->isNumerical()) {
        SemanticExn e("invalid operand in numerical expression", this->lineno);
        e.emitError();
        throw e;
    }
    return typeCheck(t1, t2, env);
}

shared_ptr<Type> NewExp::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking new expression");

    if (type->isVoid()) {
        SemanticExn e("cannot create an array of void", this->lineno);
        e.emitError();
        throw e;
    }

    auto exp_type = exp->typeCheck(env, et);
    if (!exp_type->isIntegral()) {
        SemanticExn e(
                "expression in new-declarator must have integral type",
                this->lineno);
        e.emitError();
        throw e;
    }

    Type* t = new Type(type.get());
    t->addArrDim();
    return shared_ptr<Type>(t);
}

shared_ptr<Type> MinusExp::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking minus expression");
    auto t = exp->typeCheck(env, et);
    if (!t->isNumerical()) {
        SemanticExn e("invalid operand in numerical expression", this->lineno);
        e.emitError();
        throw e;
    }
    return t;
}

shared_ptr<Type> NotExp::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking not expression");
    auto t = exp->typeCheck(env, et);
    if (!t->isBool()) {
        SemanticExn e("invalid operand in logical expression", this->lineno);
        e.emitError();
        throw e;
    }
    return t;
}

shared_ptr<Type> BoolBinaryExp::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking bool binary expression (&&, ||)");
    auto t1 = exp1->typeCheck(env, et);
    auto t2 = exp2->typeCheck(env, et);
    if (!t1->isBool() || !t2->isBool()) {
        SemanticExn e("invalid operand in logical expression", this->lineno);
        e.emitError();
        throw e;
    }
    return shared_ptr<Type>(new BoolType());
}

shared_ptr<Type> Var::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking var expression");
    // type of the identifier
    shared_ptr<Type> ident_type;
    try {
        ident_type = ident_exp->typeCheck(env, et);
    } catch (SymbolNotFoundExn& e) {
        e.lineno = this->lineno;
        e.emitError();
        throw e;
    }

    // check if the []s can be applied...
    if (((int) arr_subscripts.size()) > ident_type->getArrDim()) {
        SemanticExn e("too many []'s applied", this->lineno);
        e.emitError();
        throw e;
    }
    // ...and calculates the lvalue (var expression) type
    auto var_type = new Type(ident_type.get());
    var_type->arr_dim = ident_type->getArrDim() - arr_subscripts.size();

    // check whether all subscripts are ints
    for (auto it = arr_subscripts.items.begin(); it != arr_subscripts.items.end(); it++) {
        LOG("SUB: " << (*it)->typeCheck(env, et)->typeExp());
        if (!(*it)->typeCheck(env, et)->isIntegral()) {
            SemanticExn e("array subscript is not an integer", (*it)->lineno);
            e.emitError();
            throw e;
        }
    }

    return shared_ptr<Type>(var_type);
}

shared_ptr<Type> IfStmt::typeCheck(Env* env, shared_ptr<Type> expected_return_type) {
    LOG("typechecking if statement");
    auto cond_type = cond_exp->typeCheck(env, the_void_type());
    if (!cond_type->isBool()) {
        SemanticExn e("invalid IF condition expression", cond_exp->lineno);
        e.emitError();
        throw e;
    }
    if (!else_cmd) {
        return then_cmd->typeCheck(env, expected_return_type);
    }
    auto then_cmd_type = this->then_cmd->typeCheck(env, expected_return_type);
    auto else_cmd_type = this->else_cmd->typeCheck(env, expected_return_type);

    if (!then_cmd_type->isVoid()) {
        if (!expected_return_type->canSubstituteBy(*then_cmd_type)) {
            // TODO: not the right lineno
            ReturnTypeMismatchExn e(*expected_return_type, *then_cmd_type, this->lineno);
            e.emitError();
            throw e;
        }
    }

    if (!else_cmd_type->isVoid()) {
        if (!expected_return_type->canSubstituteBy(*else_cmd_type)) {
            // TODO: not the right lineno
            ReturnTypeMismatchExn e(*expected_return_type, *else_cmd_type, this->lineno);
            e.emitError();
            throw e;
        }
    }

    if (then_cmd_type->isVoid() || else_cmd_type->isVoid()) {
        return the_void_type();
    }

    return expected_return_type;
}

shared_ptr<Type> WhileStmt::typeCheck(Env* env, shared_ptr<Type> expected_type) {
    LOG("typechecking while statement");
    auto cond_type = cond_exp->typeCheck(env, expected_type);
    if (!cond_type->isBool()) {
        SemanticExn e("invalid WHILE condition expression", cond_exp->lineno);
        e.emitError();
        throw e;
    }
    return block->typeCheck(env, expected_type);
}

shared_ptr<Type> AssignStmt::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking assignment");
    // the expected type is ignored in this statement as it always resolves to
    // VOID
    auto var_type = var->typeCheck(env, et);
    auto rvalue_type = rvalue->typeCheck(env, et);
    if (!var_type->canSubstituteBy(*rvalue_type)) {
        InvalidAssignExn e(var_type, rvalue_type, this->lineno);
        e.emitError();
        throw e;
    }
    return the_void_type();
}

shared_ptr<Type> ReturnStmt::typeCheck(Env* env, shared_ptr<Type> expected_type) {
    LOG("typechecking return statement");
    shared_ptr<Type> e_type = (exp == NULL) ? the_void_type() : exp->typeCheck(env, expected_type);
    if (!expected_type->canSubstituteBy(*e_type)) {
        ReturnTypeMismatchExn e(*expected_type, *e_type, this->lineno);
        e.emitError();
        throw e;
    }
    return expected_type;
}

shared_ptr<Type> FuncCallStmt::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking function call statement");
    exp->typeCheck(env, et);
    return the_void_type();
}

shared_ptr<Type> Prog::typeCheck(Env* env, shared_ptr<Type> et) {
    LOG("typechecking program");
    {
        EnvScopeGuard g(env);
        for (auto it = items.begin(); it != items.end(); it++) {
            (*it)->typeCheck(env, et);
        }
    }
    return the_void_type();
}

}; // namespace monga
