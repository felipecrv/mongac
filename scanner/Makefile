CPPFLAGS=-DMG_SCANNER -DDEBUG
CXXFLAGS=-I ../src/ -I./ -Wall -O2 -std=c++0x
LEX=flex
BISON=bison --verbose
DIFF=diff -u -w -B

# depende de ast.h que define os tipos usados para definir yylval em parser.h
# depende de parser.h que define as constantes equivalentes aos tokens
mongascanner: ../src/scanner_main.cpp parser.h tokens.o ../src/ast.h
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) tokens.o $< -o $@

parser.h: ../src/parser.y
	$(BISON) --defines=$@ $<

tokens.cpp: ../src/tokens.l ../src/tokens.h
	$(LEX) -o $@ $<

clean:
	rm -f *.o *.output *.tab.c
	rm -f parser.h tokens.o tokens.cpp
	rm -f mongascanner

test: mongascanner
	./$< < ../tests/full.monga > out 2> /dev/null
	$(DIFF) out ../tests/full.monga.scan
	./$< < ../tests/basic.monga > out 2> /dev/null
	$(DIFF) out ../tests/basic.monga.scan
	./$< < ../tests/string.monga > out 2> /dev/null
	$(DIFF) out ../tests/string.monga.scan
	rm out

.PHONY: test clean
