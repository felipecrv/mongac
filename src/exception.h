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
class FuncCallExp;

class SemanticExn : public std::exception {
    private:
        bool error_emitted = false;

    protected:
        string message;

    public:
        int lineno;

        SemanticExn() { this->message = "semantic exception"; }
        SemanticExn(string msg, int lineno = -1) : message(msg), lineno(lineno) {}
        virtual const char* what() const noexcept { return this->message.c_str(); }
        bool errorEmitted() { return error_emitted; }

        virtual void emitError();
};

class DirtyScopeExn : public SemanticExn {
    virtual const char* what() const noexcept { return "DirtyScopeExn"; }
    void emitError();
};

class SymbolRedeclExn : public SemanticExn {
    public:
        SymbolRedeclExn(string, shared_ptr<Type>, shared_ptr<Type>);
};

class FuncCallArityMismatchExn : public SemanticExn {
    public:
        FuncCallArityMismatchExn(unsigned int, const FuncCallExp*);
};

class FuncCallTypeMismatchExn : public SemanticExn {
    public:
        FuncCallTypeMismatchExn(string, Type*, Type*, unsigned int, int) noexcept;
};

class ReturnTypeMismatchExn : public SemanticExn {
    public:
        ReturnTypeMismatchExn(Type, Type, int);
};

class IdentifierNotAFuncExn : public SemanticExn {
    public:
        IdentifierNotAFuncExn(string id, shared_ptr<Type>, int);
};

class InvalidAssignExn : public SemanticExn {
    public:
        InvalidAssignExn(shared_ptr<Type>, shared_ptr<Type>, int);
};

class SymbolNotFoundExn : public SemanticExn {
    public:
        SymbolNotFoundExn(const string&) noexcept;
};

class NoMatchingFuncCall : public SemanticExn {
    public:
        NoMatchingFuncCall(const FuncCallExp*, string&);
};

}; // namespace monga

#endif /* EXCEPTION_H_ */
