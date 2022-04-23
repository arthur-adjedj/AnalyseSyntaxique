%{
#include <stdio.h>

int yylex();

void yyerror(char *s)
{
        fflush(stdout);
        fprintf(stderr, "%s\n", s);
}

%}

%union {
        int   i;
}

%token <i> INT
%type <i> E

%%

S	: E		{ printf("result=%d\n",$1); }

E	: E '*' E	{ $$ = $1*$3; }
	| E '+' E	{ $$ = $1+$3; }
	| '(' E ')'	{ $$ = $2; }
	| INT

%%

#include "exprlex.c"

int main (int argc, char **argv)
{
	if (argc > 1) yyin = fopen(argv[1],"r");
	yyparse();
}

