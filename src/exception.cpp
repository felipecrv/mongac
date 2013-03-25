#include <string>
#include "exception.h"
#include "ast.h"

using namespace monga;
using namespace std;

int semantic_check_success;

void SemanticExn::emitError() {
    if (!error_emitted) {
        std::cerr << lineno << ":error: " << this->message << std::endl;
        error_emitted = true;
        semantic_check_success = 0;
    }
}

void DirtyScopeExn::emitError() {
    semantic_check_success = 0;
}

SymbolRedeclExn::SymbolRedeclExn(
        string id,
        shared_ptr<Type> fst_decl_type,
        shared_ptr<Type> snd_decl_type) {
    // lineno is set right before emitErro() call
    if (*fst_decl_type == *snd_decl_type) {
        this->message = "redeclaration of '" + id + "'";
    } else if (snd_decl_type->isFuncType()) {
        this->message = "redefinition of '" + id + "'";
    } else {
        this->message = "conflicting types for '" + id + ": " +
            snd_decl_type->typeExp() + "', previously declared as '" +
            fst_decl_type->typeExp() + "'";
    }
}

FuncCallArityMismatchExn::FuncCallArityMismatchExn(
        unsigned int func_arity,
        const FuncCallExp* func_call_exp) {
    this->lineno = func_call_exp->lineno;
    this->message = "invalid call to function '" +
        func_call_exp->func_ident->getIdentStr() + "'. It can take " +
        to_string(func_arity) + " argument(s) but passed " +
        to_string(func_call_exp->arg_exps->size());
}

FuncCallTypeMismatchExn::FuncCallTypeMismatchExn(
        string id,
        Type* expected,
        Type* passed,
        unsigned int arg_pos,
        int lineno) noexcept {
    this->lineno = lineno;
    this->message = "incompatible type for argument " +
        std::to_string(arg_pos) +
        " of '" + id + "'. Expected '" +
        expected->typeExp() + "' but argument is of type '" +
        passed->typeExp() + "'";
}

SymbolNotFoundExn::SymbolNotFoundExn(const string& symbol) noexcept {
    // lineno is set right before emitErro() call
    this->message = "'" + symbol + "' was not declared in this scope";
}

InvalidAssignExn::InvalidAssignExn(
        shared_ptr<Type> lval_type,
        shared_ptr<Type> rval_type,
        int lineno) {
    this->lineno = lineno;
    this->message = "incompatible types when assigning to type '" +
        lval_type->typeExp() + "' from type '" + rval_type->typeExp() + "'";
}

ReturnTypeMismatchExn::ReturnTypeMismatchExn(
        Type expected_type,
        Type returned_type,
        int lineno) {
    this->lineno = lineno;
    this->message = "incompatible types when returning type '" +
        returned_type.typeExp() + "' but '" + expected_type.typeExp() +
        "' was expected";
}

IdentifierNotAFuncExn::IdentifierNotAFuncExn(
        string id, shared_ptr<Type> type_instead, int lineno) {
    this->lineno = lineno;
    this->message = "'" + id + "' is not a function but a '" +
        type_instead->typeExp() + "'";
}
