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

int print_decl(decl *decl,int dec);

int link_decl_lists(decl *d1, decl *d2){
	if(d1 != NULL){
		if(d1->next == NULL){
			d1->next = d2;
		} else {
			link_decl_lists(d1->next,d2);
		}
	}

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

%token DO OD IF FI ASSIGN OR AND XOR NOT PROC VAR ELSE SKIP END REACH BREAK GT EQ PLUS MINUS TIMES CASE THEN SEMC COMMA
%token <c> IDENT
%token <e> INT

%left SEMC
%left EQ GT PLUS MINUS TIMES 
%left OR XOR
%left AND
%right NOT

%start prog 

%%

prog : declists procs specifications	{decl_list = $1; process_list = $2; specification_list = $3;}

declists : {$$ = NULL; }
    | declist declists {
		link_decl_lists($1,$2); 
		$$ = $1;
		}

declist	: VAR declarations SEMC { $$ = $2;}

procs : {$$ = NULL; }
    | PROC IDENT stmt END procs {($$ = make_proc($2,$3))->next = $5; }

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
    | declarations COMMA IDENT {($$ = make_decl_list((make_ident($3))))->next = $1; } 

cases : CASE expr THEN stmt {$$ = make_mcase(Expr,$2,$4); }
    | CASE ELSE THEN stmt {$$ = make_mcase(Else,NULL,$4); }
    | CASE expr THEN stmt cases {($$ = make_mcase(Expr,$2,$4))->next = $5; }        

assign	: IDENT ASSIGN expr 
		{ $$ = make_stmt(Assign,NULL,make_ident($1),$3,NULL,NULL,NULL); }

expr	: IDENT			{ $$ = make_expr(Ident,0,make_ident($1),NULL,NULL); }
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
    | INT 				{ $$ = $1; }





%%

#include "lexer_des_best.c"

int print_var(var *var) {
	if(var != NULL){
		printf("%s",var->name);
	} else printf(" nil ");
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

int print_mcase(mcase *mcase,int dec);

char* n_tab(int n) {
	if (n == 0) {return "";} else {
	int fullsize = n * strlen("  ") + 1;
	char* fullword;
	fullword = (char *) malloc( fullsize );
    strcpy( fullword, "" );
	strcat(fullword,"  ");
	strcat(fullword,n_tab(n-1));
	fullword;
	}
	
}

int print_stmt(stmt *stmt, int dec) {
	if(stmt != NULL){
		switch (stmt->type){
			case Assign:
				printf("%s%s := ",n_tab(dec),stmt->val.assign.var->name);
				print_expr(stmt->val.assign.expr);
			break;

			case Var:
				print_decl(stmt->val.decl,dec);
			break;

			case Semic:
				print_stmt(stmt->val.sub.left,dec);
				printf(";\n");
				print_stmt(stmt->val.sub.right,dec);
			break;	

			case Do:
				printf("%sdo \n",n_tab(dec));
				print_mcase(stmt->val.cases,dec+1);
				printf("%sod",n_tab(dec));
			break;

			case If:
				printf("%sif \n",n_tab(dec));
				print_mcase(stmt->val.cases,dec+1);
				printf("%sfi",n_tab(dec));
			break;

			case Skip:
				printf("%sskip",n_tab(dec));
			break;

			case Break:
				printf("%sbreak",n_tab(dec));
			break;
		}
	}
	return 0;
}

int print_mcase(mcase *mcase,int dec) {
	if(mcase != NULL){
		printf("%s:: ",n_tab(dec));
		if (mcase->type==Expr){
			print_expr(mcase->cond);
		}
		else printf("else");
		printf(" ->\n");
		print_stmt(mcase->command,dec+2);
		printf("\n");
		print_mcase(mcase->next,dec);
	};
	return 0;
}

int print_decl(decl *decl,int dec) {
	if(decl != NULL){
		printf("%svar %s \n",n_tab(dec),decl->var->name);
		print_decl(decl->next,dec);
	};
	return 0;
}
int print_specification(specification *spec);

int print_specification(specification *spec) {
	if(spec != NULL){
		printf("reach ");	
		print_expr(spec->expr);
		printf("\n");
		print_specification(spec->next);
	};
	return 0;
}

int print_lprocess(lprocess *proc) {
	if(proc != NULL){
		printf("proc %s \n",proc->name);
		print_stmt(proc->command,1);
		printf("\nend \n\n");
		print_lprocess(proc->next);
	};
	return 0;
}

int main (int argc, char **argv)
{
	yyin = fopen(/*argv[1]*/ "./sort.prog","r");
	yyparse();
	printf("parsing done, now printing \n");
	print_decl(decl_list,0);
	printf("\n");
	print_lprocess(process_list);
	printf("\n");
	print_specification(specification_list);
}
