
class MongaAstNode {
};

class MongaProg : public MongaAstNode {
};

class MongaDecl : public MongaAstNode {
};

class MongaVarDecls : public MongaDecl {
};

class MongaType : public MongaAstNode {
};

class MongaId : public MongaAstNode {
};

class MongaFuncDecl : public MongaDecl {
};

class MongaArg : public MongaAstNode {
};

class MongaStmt : public MongaAstNode {
};

class MongaIfStmt : public MongaStmt {
};

class MongaWhileStmt : public MongaStmt {
};

class MongaAssignStmt : public MongaStmt {
};

class MongaReturnStmt : public MongaStmt {
};

class MongaBlock : public MongaAstNode {
};

class MongaExp : public MongaAstNode {
};

class MongaVar : public MongaExp {
};

class MongaIntLiteral : public MongaExp {
};

class MongaFloatLiteral : public MongaExp {
};

class MongaStringLiteral : public MongaExp {
};

class MongaFuncCall : public MongaExp {
};

class MongaBinaryExp : public MongaExp {
};

class MongaUnaryExp : public MongaExp {
};

class MongaNewStmtExp : public MongaExp {
};
class MongaMinusExp : public MongaUnaryExp {
};
class MongaSumExp : public MongaBinaryExp {
};
class MongaSubExp : public MongaBinaryExp {
};
class MongaMultExp : public MongaBinaryExp {
};
class MongaDivExp : public MongaBinaryExp {
};
class MongaEqExp : public MongaBinaryExp {
};
class MongaLowerEqExp : public MongaBinaryExp {
};
class MongaGreaterEqExp : public MongaBinaryExp {
};
class MongaLowerExp : public MongaBinaryExp {
};
class MongaGreaterExp : public MongaBinaryExp {
};
class MongaNotExp : public MongaUnaryExp {
};
class MongaAndExp : public MongaBinaryExp {
};
class MongaOrExp : public MongaBinaryExp {
};
