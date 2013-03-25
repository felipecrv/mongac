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
        int lineno;

        SemanticExn() { this->message = "semantic exception"; }
        SemanticExn(string msg) : message(msg) {}
        virtual const char* what() const noexcept { return this->message.c_str(); }
        bool errorEmitted() { return error_emitted; }

        virtual void emitError() {
            if (!error_emitted) {
                std::cerr << "error:" << lineno << ": " << this->message << std::endl;
                error_emitted = true;
            }
        }

};

class DirtyScopeExn : public SemanticExn {
    virtual const char* what() const noexcept { return "DirtyScopeExn"; }
    virtual void emitError() {}
};

class SymbolRedeclExn : public SemanticExn {
    public:
        SymbolRedeclExn(string, shared_ptr<Type>, shared_ptr<Type>);
};

class FuncCallArityMismatchExn : public SemanticExn {
    public:
        FuncCallArityMismatchExn(string, unsigned int, unsigned int);
};

class InvalidArrSubscriptExn : public SemanticExn {
    public:
        InvalidArrSubscriptExn() {
            this->message = "array subscript is not an integer";
        }
};

class FuncCallTypeMismatchExn : public SemanticExn {
    public:
        FuncCallTypeMismatchExn(string, Type*, Type*, unsigned int) noexcept;
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
    public:
        SymbolNotFoundExn(const string&) noexcept;
};

}; // namespace monga

#endif /* EXCEPTION_H_ */
