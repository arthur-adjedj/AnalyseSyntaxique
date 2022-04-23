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

typedef struct var	// a variable
{
	char *name;
	int value;
	struct var *next;
} var;

typedef struct expr	// boolean expression
{
	int type;	// INT, OR, XOR, AND, NOT, GT, EQ, PLUS, MINUS, TIMES, 0 (variable)
    int i;
	var *var;
	struct expr *left, *right;
} expr;


typedef struct stmt	// command
{
	int type;	// ASSIGN, ';', DO, IF, SKIP, CASES
	var *var;
	expr *expr;
    struct mcase *cases; 
	struct stmt *left, *right;
} stmt;

typedef struct mcase // a case match
{
    int type; // ELSE, EXPR
    expr *cond;
    stmt *command; 
    struct mcase *next;
} mcase; 

typedef struct decl	// a variable
{
	var *var;
    struct decl *next;
} decl;





typedef struct specification // list of specifications
{
	expr *expr;
    struct specification *next; 
} specification;

typedef struct lprocess	// list of processes
{
	stmt *command;
    struct lprocess *next; 
} lprocess;

lprocess *process_list;
decl *decl_list;
specification *specification_list;

decl* make_decl_list (var *v) 
{
    decl *d = malloc(sizeof(decl));
    d->var = v;
    d->next = NULL;
    return d;
}

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
	var *v = decl_list->var;
	while (v && strcmp(v->name,s)) v = v->next;
	if (!v) { yyerror("undeclared variable"); exit(1); }
	return v;
}

expr* make_expr (int type, int n, var *var, expr *left, expr *right)
{
	expr *e = malloc(sizeof(expr));
    e->i = n;
	e->type = type;
	e->var = var;
	e->left = left;
	e->right = right;
	return e;
}

mcase* make_mcase (int type, expr *expr, stmt *stmt)
{
    mcase *c = malloc(sizeof(mcase));
    c->type = type;
    c->cond = expr;
    c->command = stmt;
    c->next = NULL;
    return c;
}

stmt* make_stmt (int type, mcase *mcase, var *var, expr *expr,
			stmt *left, stmt *right)
{
	stmt *s = malloc(sizeof(stmt));
	s->type = type;
    s->cases = mcase;
	s->var = var;
	s->expr = expr;
	s->left = left;
	s->right = right;
	return s;
}

specification* make_sp (expr *expr)
{
    specification *sp = malloc(sizeof(specification));
    sp->expr = expr;
    sp->next = NULL;
    return sp;
}

lprocess* make_proc (stmt *stmt)
{
    lprocess *p = malloc(sizeof(lprocess));
    p->command = stmt;
    p->next = NULL;
    return p;
}


%}

%union {
    specification *sp;
    lprocess *p;
    char *c;
    decl *d;
	var *v;
	expr *e;
	stmt *s;
    mcase *mc;
}

%type <sp> specifications
%type <p> procs
%type <d> declists 
%type <v> declarations declist
%type <e> expr
%type <s> stmt assign
%type <mc> cases

%token DO OD IF FI ASSIGN OR AND XOR NOT PROC VAR ELSE SKIP END REACH BREAK INT GT EQ PLUS MINUS TIMES CASE THEN
%token <c> IDENT

%left ';'

%left OR XOR
%left AND
%right NOT



%%

prog	: declists procs specifications	{decl_list = $1; process_list = $2; specification_list = $3;}

declists : {$$ = NULL; }
    | declist ';' declists {($$ = make_decl_list($1))->next = $3; }

procs : {$$ = NULL; }
    | PROC stmt END procs {($$ = make_proc($2))->next = $4; }

specifications : {$$ = NULL; }
    | REACH expr specifications {($$ = make_sp($2))->next = $3; }

declist	: VAR declarations {$$ = $2;}

declarations : IDENT { $$ = make_ident($1); }
    | declarations ',' IDENT { ($$ = make_ident($3))->next = $1; } 

stmt	: assign
	| stmt ';' stmt
		{ $$ = make_stmt(';',NULL,NULL,NULL,$1,$3); }
	| DO cases OD
		{ $$ = make_stmt(DO,$2,NULL,NULL,NULL,NULL); }
    | IF cases FI {$$ = make_stmt(IF,$2,NULL,NULL,NULL,NULL); } 
    | SKIP { $$ = make_stmt(SKIP,NULL,NULL,NULL,NULL,NULL); }
    | BREAK { $$ = make_stmt(BREAK,NULL,NULL,NULL,NULL,NULL);}

cases : CASE expr THEN stmt {$$ = make_mcase(0,$2,$4); }
    | CASE ELSE THEN stmt {$$ = make_mcase(ELSE,NULL,$4); }
    | CASE expr THEN stmt cases {($$ = make_mcase(0,$2,$4))->next = $5; }        

assign	: IDENT ASSIGN expr 
		{ $$ = make_stmt(ASSIGN,NULL,find_ident($1),$3,NULL,NULL); }

expr	: IDENT		{ $$ = make_expr(0,0,find_ident($1),NULL,NULL); }
	| expr XOR expr	{ $$ = make_expr(XOR,0,NULL,$1,$3); }
	| expr OR expr	{ $$ = make_expr(OR,0,NULL,$1,$3); }
	| expr AND expr	{ $$ = make_expr(AND,0,NULL,$1,$3); }
    | expr EQ expr {$$ = make_expr(EQ,0,NULL,$1,$3); }
    | expr PLUS expr	{ $$ = make_expr(PLUS,0,NULL,$1,$3); }
    | expr MINUS expr	{ $$ = make_expr(MINUS,0,NULL,$1,$3); }
    | expr TIMES expr	{ $$ = make_expr(TIMES,0,NULL,$1,$3); }
    | expr GT expr	{ $$ = make_expr(GT,0,NULL,$1,$3); }
	| NOT expr	{ $$ = make_expr(NOT,0,NULL,$2,NULL); }
	| '(' expr ')'	{ $$ = $2; }
    | INT 




%%

#include "lexer_des_best.c"

int main (int argc, char **argv)
{
	if (argc > 1) yyin = fopen(argv[1],"r");
	yyparse();
}
