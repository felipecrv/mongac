CXXFLAGS=-I ../src/ -I./ -Wall -O2 -std=c++0x
LEX=flex
YACC=bison
YFLAGS=--verbose --defines=parser.h
DIFF=diff -u -w -B

mongaparser: parser_main.o parser.o tokens.o
	$(CXX) $(LDFLAGS) $(LOADLIBS) $(LDLIBS) parser_main.o parser.o tokens.o -o $@

parser_main.o: ../src/parser_main.cpp ../src/ast.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<

parser.cpp: ../src/parser.y
	$(YACC) $(YFLAGS) -o $@ $<

tokens.cpp: ../src/tokens.l ../src/tokens.h
	$(LEX) -o $@ $<

clean:
	rm -f *.o *.output *.tab.c mongaparser
	rm -f parser.h parser.cpp tokens.o tokens.cpp

run: mongaparser
	./$<

test: mongaparser
	./$< < ../tests/basic.monga
	./$< < ../tests/full.monga
	./$< < ../tests/string.monga

.PHONY: test run clean
