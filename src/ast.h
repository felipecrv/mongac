class MongaProg {
};

class MongaDecl {
};

class MongaVarDecls : public MongaDecl {
};

class MongaType {
};

class MongaId {
};

class MongaFuncDecl : public MongaDecl {
};

class MongaArg {
};

class MongaStmt {
};

class MongaIfStmt : public MongaStmt {
};

class MongaWhileStmt : public MongaStmt {
};

class MongaAssignStmt : public MongaStmt {
};

class MongaReturnStmt : public MongaStmt {
};

class MongaBlock {
};

class MongaExp {
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
