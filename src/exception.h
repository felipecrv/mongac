#ifndef EXCEPTION_H_
#define EXCEPTION_H_

#include <exception>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#define WARN(ops)  std::cerr << "warning: " << ops << std::endl
#define ERROR(ops)  std::cerr << "error: " << ops << std::endl

namespace monga {
using namespace std;

class Type;

class SemanticExn : public std::exception {
    private:
        bool error_emitted = false;

    protected:
        string message;

    public:
        SemanticExn() { this->message = "semantic exception"; }
        SemanticExn(string msg) : message(msg) {}
        const char* what() const noexcept { return this->message.c_str(); }
        bool errorEmitted() { return error_emitted; }

        void emitError() {
            if (!error_emitted) {
                std::cerr << "error: " << this->message << std::endl;
                error_emitted = true;
            }
        }

};

class MissingSymExn : public SemanticExn {
};

class SymbolRedeclExn : public SemanticExn {
    public:
        string ident;
        shared_ptr<Type> fst_decl_type;
        shared_ptr<Type> snd_decl_type;

        SymbolRedeclExn(string, shared_ptr<Type>, shared_ptr<Type>);
};

class FuncCallArityMismatchExn : public SemanticExn {
    public:
        FuncCallArityMismatchExn(string s, unsigned int func_n, unsigned int call_n) {
            this->message = "invalid call to function '" + s + "'. It can take " +
                to_string(func_n) + " argument(s) but passed " + to_string(call_n);
        }
};

class InvalidArrSubscriptExn : public SemanticExn {
    public:
        InvalidArrSubscriptExn() {
            this->message = "array subscript is not an integer";
        }
};

class FuncCallTypeMismatchExn : public SemanticExn {
    private:
        string ident;
        shared_ptr<Type> expected_type;
        shared_ptr<Type> passed_type;
        unsigned int arg_pos;

    public:
        FuncCallTypeMismatchExn(string, Type*, Type*, unsigned int);
};

class ReturnTypeMismatchExn : public SemanticExn {
    public:
        ReturnTypeMismatchExn(Type, Type);
};

class NonOrdTypeComparisonExn : public SemanticExn {
};

class IdentifierNotAFuncExn : public SemanticExn {
};

class InvalidAssignExn : public SemanticExn {
    private:
        shared_ptr<Type> lvalue_type;
        shared_ptr<Type> rvalue_type;

    public:
        InvalidAssignExn(shared_ptr<Type>, shared_ptr<Type>);
};

class InvalidOperandTypeExn : public SemanticExn {
};

class NonBoolCondExn : public SemanticExn {
};

class NonIntegralAllocationSizeExn : public SemanticExn {
    public:
        NonIntegralAllocationSizeExn() {
            this->message =
                "expression in new-declarator must have integral type";
        }
};

class NonNumericalOperandExn : public SemanticExn {
};

class NonEqTypeComparisonExn : public SemanticExn {
};

class SymbolNotFoundExn : public SemanticExn {
    private:
        string symbol;

    public:
        SymbolNotFoundExn(const string&) noexcept;
};

}; // namespace monga

#endif /* EXCEPTION_H_ */
