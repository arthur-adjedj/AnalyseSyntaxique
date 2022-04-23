%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yylex();

void yyerror(char *s)
{
	fflush(stdout);
	fprintf(stderr, "%s\n", s);
}

typedef struct expr	// boolean expression
{
	int type;	// TRUE, FALSE, OR, AND, NOT, 0 (variable)
	var *var;
	struct expr *left, *right;
} expr;

%}



%union {
	int i;
    char *c;
	var *v;
	varlist *l;
	expr *e;
	stmt *s;
}

%type <v> declist
%type <l> varlist
%type <e> expr
%type <s> stmt assign

%token BOOL WHILE DO OD ASSIGN PRINT OR AND XOR NOT TRUE FALSE
%token <i> IDENT

%left ';'

%left OR XOR
%left AND
%right NOT



%%






%%

