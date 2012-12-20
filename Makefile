all:
	flex src/monga.l
	#gcc -lfl lex.yy.c -o mongascan
	#gcc -DDEBUG lex.yy.c -o mongascan
	gcc lex.yy.c -o mongascan

test: all
	./mongascan < tests/full.monga > out 2> /dev/null
	diff -u -w -B out tests/full.monga.scan
	./mongascan < tests/basic.monga > out 2> /dev/null
	diff -u -w -B out tests/basic.monga.scan
	./mongascan < tests/string.monga > out 2> /dev/null
	diff -u -w -B out tests/string.monga.scan
	rm out

