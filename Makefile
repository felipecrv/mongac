CPP=g++
CPP_FLAGS=-Wall -O2 -std=c++0x -DDEBUG
LEX=flex
DIFF=diff -u -w -B

mongascan: lex.yy.cpp
	$(CPP) $(CPP_FLAGS) $< -o $@

lex.yy.cpp: src/monga.l
	$(LEX) -o $@ $<

clean:
	rm -f lex.yy.*
	rm -f mongascan

test: mongascan
	./$< < tests/full.monga > out 2> /dev/null
	$(DIFF) out tests/full.monga.scan
	./$< < tests/basic.monga > out 2> /dev/null
	$(DIFF) out tests/basic.monga.scan
	./$< < tests/string.monga > out 2> /dev/null
	$(DIFF) out tests/string.monga.scan
	rm out

.PHONY: test
