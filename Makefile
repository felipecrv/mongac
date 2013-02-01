CC=gcc
LEX=flex
DIFF=diff -u -w -B

mongascan: lex.yy.c
	$(CC) $< -o $@

lex.yy.c: src/monga.l
	$(LEX) $<

test: mongascan
	./$< < tests/full.monga > out 2> /dev/null
	$(DIFF) out tests/full.monga.scan
	./$< < tests/basic.monga > out 2> /dev/null
	$(DIFF) out tests/basic.monga.scan
	./$< < tests/string.monga > out 2> /dev/null
	$(DIFF) out tests/string.monga.scan
	rm out

.PHONY: test
