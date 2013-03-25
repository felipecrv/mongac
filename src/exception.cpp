#include <string>
#include "exception.h"
#include "ast.h"

using namespace monga;
using namespace std;

SymbolRedeclExn::SymbolRedeclExn(
        string id,
        shared_ptr<Type> fst_decl_type,
        shared_ptr<Type> snd_decl_type) {
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
        string func_name,
        unsigned int func_n,
        unsigned int call_n) {
    this->message = "invalid call to function '" + func_name + "'. It can take " +
        to_string(func_n) + " argument(s) but passed " + to_string(call_n);
}

FuncCallTypeMismatchExn::FuncCallTypeMismatchExn(
        string id,
        Type* expected,
        Type* passed,
        unsigned int arg_pos) noexcept {
    this->message = "incompatible type for argument " +
        std::to_string(arg_pos) +
        " of '" + id + "'. Expected '" +
        expected->typeExp() + "' but argument is of type '" +
        passed->typeExp() + "'";
}

SymbolNotFoundExn::SymbolNotFoundExn(const string& symbol) noexcept {
    this->message = "'" + symbol + "' was not declared in this scope";
}

InvalidAssignExn::InvalidAssignExn(shared_ptr<Type> lval_type, shared_ptr<Type> rval_type) {
    this->message = "incompatible types when assigning to type '" +
        lval_type->typeExp() + "' from type '" + rval_type->typeExp() + "'";
}

ReturnTypeMismatchExn::ReturnTypeMismatchExn(Type expected_type, Type returned_type) {
    this->message = "incompatible types when returning type '" +
        returned_type.typeExp() + "' but '" + expected_type.typeExp() +
        "' was expected";
}
