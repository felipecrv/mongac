#include <string>
#include "exception.h"
#include "ast.h"

using namespace monga;
using namespace std;

SymbolRedeclExn::SymbolRedeclExn(
        string id,
        shared_ptr<Type> fst_decl_t,
        shared_ptr<Type> snd_decl_t)
            : ident(id),
            fst_decl_type(fst_decl_t),
            snd_decl_type(snd_decl_t) {
    if (*fst_decl_type == *snd_decl_type) {
        this->message = "redeclaration of '" + this->ident + "'";
    } else if (snd_decl_type->isFuncType()) {
        this->message = "redefinition of '" + this->ident + "'";
    } else {
        this->message = "conflicting types for '" +
            this->ident + ": " +
            snd_decl_type->typeExp() +
            "', previously declared as '" +
            fst_decl_type->typeExp() + "'";
    }
}

FuncCallTypeMismatchExn::FuncCallTypeMismatchExn(
        string id,
        Type* expected,
        Type* passed,
        unsigned int arg_pos)
            : ident(id),
            expected_type(shared_ptr<Type>(expected)),
            passed_type(shared_ptr<Type>(passed)),
            arg_pos(arg_pos) {
    this->message = "incompatible type for argument " +
        std::to_string(this->arg_pos) +
        " of '" + this->ident + "'. Expected '" +
        this->expected_type->typeExp() + "' but argument is of type '" +
        this->passed_type->typeExp() + "'";
}

SymbolNotFoundExn::SymbolNotFoundExn(const string& symbol) noexcept
        : symbol(symbol) {
    this->message = "'" + symbol + "' was not declared in this scope";
}

InvalidAssignExn::InvalidAssignExn(shared_ptr<Type> lval_type, shared_ptr<Type> rval_type)
        : lvalue_type(lval_type), rvalue_type(rval_type) {
    this->message = "incompatible types when assigning to type '" +
        lval_type->typeExp() + "' from type '" + rval_type->typeExp() + "'";
}

ReturnTypeMismatchExn::ReturnTypeMismatchExn(Type expected_type, Type returned_type) {
    this->message = "incompatible types when returning type '" +
        returned_type.typeExp() + "' but '" + expected_type.typeExp() +
        "' was expected";
}
