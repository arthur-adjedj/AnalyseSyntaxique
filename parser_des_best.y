%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


enum type_expr{Ident, Int, Or, Xor, And, Not, Gt, Eq, Plus, Minus, Times, Zero};
enum type_stmt{Assign, Semic, Do, If, Skip, Cases} ;
enum type_mcase{Else, Expr};

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
	enum type_expr type;	// INT, OR, XOR, AND, NOT, GT, EQ, PLUS, MINUS, TIMES, 0 (variable)
    int i;
	var *var;
	struct expr *left, *right;
} expr;


typedef struct stmt	// command
{
	enum type_stmt type;	// ASSIGN, ';', DO, IF, SKIP, CASES
	var *var;
	expr *expr;
    struct mcase *cases; 
	struct stmt *left, *right;
} stmt;

typedef struct mcase // a case match
{
    enum type_mcase type; // ELSE, EXPR
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

expr* make_expr (enum type_expr type, int n, var *var, expr *left, expr *right)
{
	expr *e = malloc(sizeof(expr));
    e->i = n;
	e->type = type;
	e->var = var;
	e->left = left;
	e->right = right;
	return e;
}

mcase* make_mcase (enum type_mcase type, expr *expr, stmt *stmt)
{
    mcase *c = malloc(sizeof(mcase));
    c->type = type;
    c->cond = expr;
    c->command = stmt;
    c->next = NULL;
    return c;
}

stmt* make_stmt (enum type_stmt type, mcase *mcase, var *var, expr *expr,
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

%token DO OD IF FI ASSIGN OR AND XOR NOT PROC VAR ELSE SKIP END REACH BREAK INT GT EQ PLUS MINUS TIMES CASE THEN SEMC COMMA
%token <c> IDENT

%left SEMC
%left EQ GT PLUS MINUS TIMES 
%left OR XOR
%left AND
%right NOT

%start prog 

%%

prog : declists procs specifications	{decl_list = $1; process_list = $2; specification_list = $3;}

declists : {$$ = NULL; }
    | declist declists {($$ = make_decl_list($1))->next = $2; }

procs : {$$ = NULL; }
    | PROC IDENT stmt END procs {($$ = make_proc($3))->next = $5; }

specifications : {$$ = NULL; }
    | REACH expr specifications {($$ = make_sp($2))->next = $3; }

declist	: VAR declarations SEMC {$$ = $2;}

declarations : IDENT { $$ = make_ident($1); }
    | declarations COMMA IDENT { ($$ = make_ident($3))->next = $1; } 

stmt	: assign
	| stmt SEMC stmt
		{ $$ = make_stmt(SEMC,NULL,NULL,NULL,$1,$3); }
	| DO cases OD
		{ $$ = make_stmt(DO,$2,NULL,NULL,NULL,NULL); }
    | IF cases FI {$$ = make_stmt(IF,$2,NULL,NULL,NULL,NULL); } 
    | SKIP { $$ = make_stmt(SKIP,NULL,NULL,NULL,NULL,NULL); }
    | BREAK { $$ = make_stmt(BREAK,NULL,NULL,NULL,NULL,NULL);}

cases : CASE expr THEN stmt {$$ = make_mcase(0,$2,$4); }
    | CASE ELSE THEN stmt {$$ = make_mcase(ELSE,NULL,$4); }
    | CASE expr THEN stmt cases {($$ = make_mcase(0,$2,$4))->next = $5; }        

assign	: IDENT ASSIGN expr 
		{ $$ = make_stmt(ASSIGN,NULL,NULL,$3,NULL,NULL); }

expr	: IDENT			{ $$ = make_expr(Ident,0,NULL,NULL,NULL); }
	| expr XOR expr		{ $$ = make_expr(Xor,0,NULL,$1,$3); }
	| expr OR expr		{ $$ = make_expr(Or,0,NULL,$1,$3); }
	| expr AND expr		{ $$ = make_expr(And,0,NULL,$1,$3); }
    | expr EQ expr 		{$$ = make_expr(Eq,0,NULL,$1,$3); }
    | expr PLUS expr	{ $$ = make_expr(Plus,0,NULL,$1,$3); }
    | expr MINUS expr	{ $$ = make_expr(Minus,0,NULL,$1,$3); }
    | expr TIMES expr	{ $$ = make_expr(Times,0,NULL,$1,$3); }
    | expr GT expr		{ $$ = make_expr(Gt,0,NULL,$1,$3); }
	| NOT expr			{ $$ = make_expr(Not,0,NULL,$2,NULL); }
	| '(' expr ')'		{ $$ = $2; }
    | INT 				{ $$ = yylval.e; }





%%

#include "lexer_des_best.c"

int print_var(var var) {
	printf("var %s = %d \n",var.name,var.value);
	if (var.next != NULL){
		print_var(*var.next);
	};
	return 0;
}

int print_expr(expr expr) {
	return 0;
}

int print_stmt() {
	return 0;
}

int print_mcase() {
	return 0;
}

int print_decl() {
	return 0;
}

int print_specification() {
	return 0;
}

int print_lprocess() {
	return 0;
}

int main (int argc, char **argv)
{
	if (argc > 1) yyin = fopen(argv[1],"r");
	yyparse();
}
