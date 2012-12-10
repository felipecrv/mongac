/* scanner para a linguagem Monga */

%{
extern int yywrap(void) {
	return 1;
}

int num_lines = 0, num_chars = 0, current_col = 0;
%}

%%
\n      ++num_lines; ++num_chars; current_col = 0;
.       ++num_chars; ++current_col;
%%


int main(int argc, char *argv[]) {
	yylex();
	printf("%d %d %d\n", num_lines, num_chars, current_col);
	return 0;
}
