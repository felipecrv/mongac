#ifndef CODEGEN_H_
#define CODEGEN_H_

#include <iostream>
#include "ast.h"

namespace monga {

class CodeGen {
public:
    virtual void gen(const monga::Prog*, std::ostream& os) = 0;
};

};

#endif /* CODEGEN_H_ */
