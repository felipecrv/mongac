#ifndef EXCEPTION_H_
#define EXCEPTION_H_

#include <exception>
#include <memory>
#include <string>

#define WARN(ops)  std::cerr << "warning: " << ops << std::endl
#define ERROR(ops)  std::cerr << "error: " << ops << std::endl
#define FATAL(ops)  std::cerr << "error: " << ops << std::endl; {FatalErrorExn e; throw e;}

namespace monga {
using namespace std;

class Type;

class SemanticExn : public std::exception {
};

class FatalErrorExn : public std::exception {
};

class MissingSymExn : public std::exception {
};

class SymbolRedeclExn : public SemanticExn {
    public:
        shared_ptr<Type> fst_decl_type;
        shared_ptr<Type> snd_decl_type;

        SymbolRedeclExn(shared_ptr<Type> fst_decl_t, shared_ptr<Type> snd_decl_t)
            : fst_decl_type(fst_decl_t), snd_decl_type(snd_decl_t) {}
};

class FuncCallArityMismatchExn : public SemanticExn {
};

class InvalidArrSubscriptExn : public SemanticExn {
};

class FuncCallTypeMismatchExn : public SemanticExn {
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
        InvalidAssignExn(shared_ptr<Type> lval_type, shared_ptr<Type> rval_type)
            : lvalue_type(lval_type), rvalue_type(rval_type) {
        }

        const char* what() const noexcept;
};

class InvalidOperandTypeExn : public SemanticExn {
};

class NonBoolCondExn : public SemanticExn {
};

class NonIntegralAllocationSizeExn : public SemanticExn {
};

class NonNumericalOperandExn : public SemanticExn {
};

class NonEqTypeComparisonExn : public SemanticExn {
};

class SymbolNotFoundExn : public SemanticExn {
    private:
        string symbol;

    public:
        SymbolNotFoundExn(const string& symbol) noexcept : symbol(symbol) {}

        const char* what() const noexcept {
            string s = "symbol " + symbol + " not found in this scope";
            return s.c_str();
        }
};

}; // namespace monga

#endif /* EXCEPTION_H_ */
