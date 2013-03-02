CXXFLAGS=-I ../src/ -I./ -Wall -O2 -std=c++0x
LEX=flex
YACC=bison
YFLAGS=--verbose --defines=parser.h
DIFF=diff -u -w -B

mongaparser: parser_main.o parser.o tokens.o ast.o exception.o
	$(CXX) $(LDFLAGS) $(LOADLIBS) $(LDLIBS) parser_main.o parser.o tokens.o ast.o exception.o -o $@

parser_main.o: ../src/parser_main.cpp ../src/ast.h parser.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<

ast.o: ../src/ast.cpp ../src/ast.h parser.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<

exception.o: ../src/exception.cpp ../src/ast.h ../src/exception.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<

%.o: %.cpp ../src/ast.h parser.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<

parser.h: parser.cpp

parser.cpp: ../src/parser.y ../src/ast.h
	$(YACC) $(YFLAGS) -o $@ $<

tokens.cpp: ../src/tokens.l ../src/tokens.h
	$(LEX) -o $@ $<

clean:
	rm -f *.o *.output *.tab.c mongaparser
	rm -f parser.h parser.cpp tokens.o tokens.cpp

test: mongaparser
	./$< < ../tests/basic.monga
	./$< < ../tests/full.monga
	./$< < ../tests/string.monga
	./$< < ../tests/exps.monga
	./$< --dump-ast < ../tests/basic.monga > /dev/null
	./$< --dump-ast < ../tests/full.monga > /dev/null
	./$< --dump-ast < ../tests/string.monga > /dev/null
	./$< --dump-ast < ../tests/exps.monga > /dev/null
	./$< --dump-ast < ../tests/exps.monga > out.ast
	$(DIFF) ../tests/exps.monga.ast out.ast
	rm -f out.ast

.PHONY: test run clean
