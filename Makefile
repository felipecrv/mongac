CPP=g++
CPP_FLAGS=-I src/ -I /usr/include/c++/4.7/ -Wall -O2 -std=c++0x -DDEBUG
LEX=flex
BISON=bison --verbose
DIFF=diff -u -w -B

all: mongascanner mongaast

tokens.cpp: src/tokens.l
	$(LEX) -o $@ $<

# depende de ast.h que define os tipos usados para definir yylval em parser.h
# depende de parser.h que define as constantes equivalentes aos tokens
tokens.o: tokens.cpp src/ast.h parser.h
	$(CPP) $(CPP_FLAGS) -c $< -o $@

mongascanner: src/scanner.cpp tokens.cpp
	# temos que recompilar tokens.o definindo MG_SCANNER
	$(CPP) $(CPP_FLAGS) -DMG_SCANNER -c tokens.cpp -o tokens.o
	$(CPP) $(CPP_FLAGS) src/scanner.cpp tokens.o -o $@

parser.cpp: src/parser.y
	$(BISON) --defines=parser.h -o $@ src/parser.y

parser.h: parser.cpp

parser.o: parser.cpp parser.h
	$(CPP) $(CPP_FLAGS) -c $< -o $@

mongaast: parser.o tokens.o
	$(CPP) $(CPP_FLAGS) parser.o tokens.o src/ast_main.cpp -o $@

clean:
	rm -f *.o
	rm -f tokens.cpp
	rm -f mongascanner

test_scanner: mongascanner
	./$< < tests/full.monga > out 2> /dev/null
	$(DIFF) out tests/full.monga.scan
	./$< < tests/basic.monga > out 2> /dev/null
	$(DIFF) out tests/basic.monga.scan
	./$< < tests/string.monga > out 2> /dev/null
	$(DIFF) out tests/string.monga.scan
	rm out

test_parser: mongaast
	./$< < tests/full.monga

test: test_scanner

.PHONY: all test_scanner clean
