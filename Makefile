CPP=g++
CPP_FLAGS=-I src/ -Wall -O2 -std=c++0x -DDEBUG
LEX=flex
DIFF=diff -u -w -B

tokens.cpp: src/tokens.l
	$(LEX) -o $@ $<

tokens.o: tokens.cpp
	$(CPP) $(CPP_FLAGS) -c $< -o $@

mongascanner: src/scanner.cpp tokens.o
	$(CPP) $(CPP_FLAGS) src/scanner.cpp tokens.o -o $@

clean:
	rm -f *.o
	rm -f tokens.cpp
	rm -f mongascanner

test: mongascanner
	./$< < tests/full.monga > out 2> /dev/null
	$(DIFF) out tests/full.monga.scan
	./$< < tests/basic.monga > out 2> /dev/null
	$(DIFF) out tests/basic.monga.scan
	./$< < tests/string.monga > out 2> /dev/null
	$(DIFF) out tests/string.monga.scan
	rm out

.PHONY: test clean
