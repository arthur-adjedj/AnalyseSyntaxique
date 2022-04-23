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
	int type;	// TRUE, FALSE, INT, OR, XOR, AND, NOT, GT, EQ, PLUS, MINUS, TIMES, 0 (variable)
	var *var;
	struct expr *left, *right;
} expr;

typedef struct var	// a variable
{
	char *name;
	int value;
	struct var *next;
} var;

typedef struct stmt	// command
{
	int type;	// ASSIGN, SEMC, DO, IF, SKIP
	var *var;
	expr *expr;
	struct stmt *left, *right;
} stmt;

var *program_vars;
stmt *program_stmts;

var* make_ident (char *s)
{
	var *v = malloc(sizeof(var));
	v->name = s;
	v->value = 0;	// make variable false initially
	v->next = NULL;
	return v;
}

var* find_ident (char *s)
{
	var *v = program_vars;
	while (v && strcmp(v->name,s)) v = v->next;
	if (!v) { yyerror("undeclared variable"); exit(1); }
	return v;
}

expr* make_expr (int type, var *var, expr *left, expr *right)
{
	expr *e = malloc(sizeof(expr));
	e->type = type;
	e->var = var;
	e->left = left;
	e->right = right;
	return e;
}

stmt* make_stmt (int type, var *var, expr *expr,
			stmt *left, stmt *right, varlist *list)
{
	stmt *s = malloc(sizeof(stmt));
	s->type = type;
	s->var = var;
	s->expr = expr;
	s->left = left;
	s->right = right;
	return s;
}


%}

%union {
    char *c;
	var *v;
	expr *e;
	stmt *s;
}

%type <v> declist
%type <e> expr
%type <s> stmt assign

%token DO OD IF FI ASSIGN OR AND XOR NOT TRUE FALSE PROC VAR ELSE SKIP END REACH BREAK INT GT EQ PLUS MINUS TIMES
%token <i> IDENT

%left ';'

%left OR XOR
%left AND
%right NOT



%%






%%

