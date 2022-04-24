%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum type_expr{Ident, Int, Or, Xor, And, Not, Gt, Eq, Plus, Minus, Times, Zero};
enum type_stmt{Assign, Semic, Do, If, Skip, Break, Var} ;
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
	int val;
} var;

typedef struct expr	// boolean expression
{
	enum type_expr type;	//Ident (variable), Int, Or, Xor, And, Not, Gt, Eq, Plus, Minus Times
	union {
    	int i;
		var *var;
		struct {
			struct expr *left, *right;
		} sub;
	} val;
} expr;


typedef struct stmt	// command
{
	enum type_stmt type;	// Assign, Semic, Do, If, Skip, Break, Var
	union {
		struct {
			var *var;
			expr *expr;
		} assign;
		struct mcase *cases;
		struct {
			struct stmt *left, *right;
		} sub;
		struct decl *decl;

	} val;
} stmt;

typedef struct mcase // a case match
{
    enum type_mcase type; // Else, Expr
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
	char *name;
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

int link_decl_lists(decl *d1, decl *d2){
	if(d1->next = NULL){
		d1->next = d2;
	} else link_decl_lists(d1->next,d2);

}

var* make_ident (char *s)
{
	var *v = malloc(sizeof(var));
	v->name = s;
	v->val = 0;	// make variable false initially
	return v;
}
/*
var* find_ident (char *s)
{
	var *v = decl_list->var;
	while (v && strcmp(v->name,s)) v = v->next;
	if (!v) { yyerror("undeclared variable"); exit(1); }
	return v;
}
*/

expr* make_expr (enum type_expr type, int n, var *var, expr *left, expr *right)
{
	expr *e = malloc(sizeof(expr));
	e->type = type;
	switch(type){
		case Ident:
			e->val.var = var;
		break;
		
		case Int:
			e->val.i = n;
		break;

		default:
			e->val.sub.left = left;
			e->val.sub.right = right;
		break;
	};
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
			stmt *left, stmt *right, decl *decl)
{
	stmt *s = malloc(sizeof(stmt));
	s->type = type;
	switch(type){
		case Assign:
			s->val.assign.var = var;
			s->val.assign.expr = expr;
		break;

		case Semic:
				s->val.sub.left = left;
				s->val.sub.right = right;
		break;

		case Var:
			s->val.decl = decl;
		break;

		default:
			    s->val.cases = mcase;
		break;
	}
	return s;
}

specification* make_sp (expr *expr)
{
    specification *sp = malloc(sizeof(specification));
    sp->expr = expr;
    sp->next = NULL;
    return sp;
}

lprocess* make_proc (char *name,stmt *stmt)
{
    lprocess *p = malloc(sizeof(lprocess));
	p->name = name;
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
	decl *v;
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

declists : {printf("ahah\n"); $$ = NULL; }
    | declist declists {printf("youpi \n"); link_decl_lists($$ = $1,$2); }

declist	: VAR declarations SEMC {printf("var\n"); $$ = $2;}

procs : {$$ = NULL; }
    | PROC IDENT stmt END procs {printf("hihihi\n"); ($$ = make_proc($2,$3))->next = $5; }

specifications : {$$ = NULL; }
    | REACH expr specifications {($$ = make_sp($2))->next = $3; }

stmt	: assign
	| VAR declarations { $$ = make_stmt(Var,NULL,NULL,NULL,NULL,NULL,$2); }
	| stmt SEMC stmt
		{ $$ = make_stmt(Semic,NULL,NULL,NULL,$1,$3,NULL); }
	| DO cases OD
		{ $$ = make_stmt(Do,$2,NULL,NULL,NULL,NULL,NULL); }
    | IF cases FI {$$ = make_stmt(If,$2,NULL,NULL,NULL,NULL,NULL); } 
    | SKIP { $$ = make_stmt(Skip,NULL,NULL,NULL,NULL,NULL,NULL); }
    | BREAK { $$ = make_stmt(Break,NULL,NULL,NULL,NULL,NULL,NULL);}

declarations : IDENT { $$ = make_decl_list(make_ident($1)); }
    | declarations COMMA IDENT { ($$ = make_decl_list((make_ident($3))))->next = $1; } 

cases : CASE expr THEN stmt {$$ = make_mcase(0,$2,$4); }
    | CASE ELSE THEN stmt {$$ = make_mcase(ELSE,NULL,$4); }
    | CASE expr THEN stmt cases {($$ = make_mcase(0,$2,$4))->next = $5; }        

assign	: IDENT ASSIGN expr 
		{ $$ = make_stmt(Assign,NULL,NULL,$3,NULL,NULL,NULL); }

expr	: IDENT			{ printf("expr_ident"); $$ = make_expr(Ident,0,NULL,NULL,NULL); }
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

int print_var(var *var) {
	if(var != NULL){
		printf("%s",var->name);
	}
	return 0;
}

int print_expr(expr *expr) {
	if(expr != NULL){
		switch (expr->type)
		{
			case Ident:
				printf("%s", expr->val.var->name);
			break;

			case Int:
				printf("%d", expr->val.i);
			break;

			case Or:
				print_expr(expr->val.sub.left);
				printf(" || ");
				print_expr(expr->val.sub.right);
			break;

			case And:
				print_expr(expr->val.sub.left);
				printf(" && ");
				print_expr(expr->val.sub.right);
			break;

			case Xor:
				print_expr(expr->val.sub.left);
				printf(" ^ ");
				print_expr(expr->val.sub.right);
			break;

			case Gt:
				print_expr(expr->val.sub.left);
				printf(" > ");
				print_expr(expr->val.sub.right);
			break;

			case Eq:
				print_expr(expr->val.sub.left);
				printf(" == ");
				print_expr(expr->val.sub.right);
			break;

			case Plus:
				print_expr(expr->val.sub.left);
				printf(" + ");
				print_expr(expr->val.sub.right);
			break;

			case Minus:
				print_expr(expr->val.sub.left);
				printf(" - ");
				print_expr(expr->val.sub.right);
			break;

			case Times:
				print_expr(expr->val.sub.left);
				printf(" * ");
				print_expr(expr->val.sub.right);
			break;

			case Not:
				printf("~");
				print_expr(expr->val.sub.left);
			break;
		}
	}	
	return 0;
}

int print_mcase(mcase *mcase);

int print_stmt(stmt *stmt) {
	if(stmt != NULL){
		switch (stmt->type){
			case Assign:
				printf("%s := ",stmt->val.assign.var->name);
				print_expr(stmt->val.assign.expr);
			break;

			case Semic:
				print_stmt(stmt->val.sub.left);
				printf(";\n");
				print_stmt(stmt->val.sub.right);
			break;	

			case Do:
				printf("do \n");
				print_mcase(stmt->val.cases);
				printf("od \n");
			break;

			case If:
				printf("if \n");
				print_mcase(stmt->val.cases);
				printf("fi \n");
			break;

			case Skip:
				printf("skip");
			break;

			case Break:
				printf("break");
			break;
		}
	}
	return 0;
}

int print_mcase(mcase *mcase) {
	if(mcase != NULL){
		printf(":: ");
		if (mcase->type==Expr){
			print_expr(mcase->cond);
		}
		else printf("else");
		printf(" -> ");
		print_stmt(mcase->command);
		printf("\n");
		print_mcase(mcase->next);
	};
	return 0;
}

int print_decl(decl *decl) {
	if(decl != NULL){
		printf("var %s \n",decl->var->name);
		print_decl(decl->next);
	}
	return 0;
}

int print_specification(specification *spec) {
	if(spec != NULL){
		printf("reach ");	
		print_expr(spec->expr);
		print_specification(spec->next);
	};
	return 0;
}

int print_lprocess(lprocess *proc) {
	if(proc != NULL){
		printf("proc %s \n",proc->name);
		print_stmt(proc->command);
		printf("end \n");
		print_lprocess(proc->next);
	};
	return 0;
}

int main (int argc, char **argv)
{
	if (argc > 1) yyin = fopen(/*argv[1]*/ "./test2","r");
	yyparse();
	printf("parsing done, now printing");
	printf("%p",decl_list);
	print_decl(decl_list);
	print_lprocess(process_list);
	print_specification(specification_list);
}
