CXXFLAGS=-I ../src/ -I./ -Wall -g -std=c++0x
LEX=flex
YACC=bison
YFLAGS=--verbose --defines=parser.h
DIFF=diff -u -w -B

OBJS=mongac_main.o parser.o tokens.o ast.o exception.o PascalCodeGen.o

mongac: $(OBJS)
	$(CXX) $(LDFLAGS) $(LOADLIBS) $(LDLIBS) $(OBJS) -o $@

mongac_main.o: ../src/mongac_main.cpp ../src/ast.h parser.h ../src/PascalCodeGen.h
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

PascalCodeGen.o: ../src/PascalCodeGen.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<
	
clean:
	rm -f *.o *.output *.tab.c mongac
	rm -f parser.h parser.cpp tokens.o tokens.cpp

test: mongac
	./$<

.PHONY: test run clean
