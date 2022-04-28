%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum type_expr{Ident, Int, Or, Xor, And, Not, Gt, Eq, Plus, Minus, Times, Zero};
enum type_stmt{Assign, Semic, Do, If, Skip, Break} ;
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
	enum type_stmt type;	// Assign, Semic, Do, If, Skip, Break
	union {
		struct {
			var *var;
			expr *expr;
		} assign;
		struct mcase *cases;
		struct {
			struct stmt *left, *right;
		} sub;

	} val;
	int youre_here;  // si l'evaluation du processus en est à cette étape
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
	struct stmt *command;
    struct lprocess *next; 
	struct decl *vars;
	int alive;
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

var* find_ident_d (char *s,decl *d_start)
{
	struct decl *d = d_start; 
	while (d && strcmp(d->var->name,s)) d = d->next;
	if (!d) { return NULL; }
	return d->var;
}

var* find_ident_p (char *s, lprocess *p) 
{
	struct var *first_try = find_ident_d(s,decl_list);
	if (!first_try)
	{
		struct var *second_try = find_ident_d(s,p->vars);
		if (!second_try) 
		{
			yyerror("cette variable n'existe pas");
		}
		else
		{
			return second_try;
		}
	}
	else
	{
		return first_try;
	}

}


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
			stmt *left, stmt *right)
{
	stmt *s = malloc(sizeof(stmt));
	s->type = type;
	s->youre_here = 0;
	switch(type){
		case Assign:
			s->val.assign.var = var;
			s->val.assign.expr = expr;
		break;

		case Semic:
				s->val.sub.left = left;
				s->val.sub.right = right;
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

lprocess* make_proc (char *name,stmt *stmt, decl *decl)
{
    lprocess *p = malloc(sizeof(lprocess));
	p->name = name;
    p->command = stmt;
    p->next = NULL;
	p->vars = decl;
	p->alive = 1;
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

prog : declists procs specifications	{ decl_list = $1; process_list = $2; specification_list = $3;}

procs : {$$ = NULL; }
    | PROC IDENT declists stmt END procs {($$ = make_proc($2,$4,$3))->next = $6; }

declists : {$$ = NULL; }
    | declist declists { link_decl_lists($1,$2); $$ = $1; }

declist	: VAR declarations SEMC { $$ = $2;}

specifications : {$$ = NULL; }
    | REACH expr specifications {($$ = make_sp($2))->next = $3; }

stmt	: assign
	| stmt SEMC stmt
		{ $$ = make_stmt(Semic,NULL,NULL,NULL,$1,$3); }
	| DO cases OD
		{ $$ = make_stmt(Do,$2,NULL,NULL,NULL,NULL); }
    | IF cases FI {$$ = make_stmt(If,$2,NULL,NULL,NULL,NULL); } 
    | SKIP { $$ = make_stmt(Skip,NULL,NULL,NULL,NULL,NULL); }
    | BREAK { $$ = make_stmt(Break,NULL,NULL,NULL,NULL,NULL);}

declarations : IDENT { $$ = make_decl_list(make_ident($1)); }
    | declarations COMMA IDENT {($$ = make_decl_list((make_ident($3))))->next = $1; } 

cases : CASE expr THEN stmt {$$ = make_mcase(Expr,$2,$4); }
    | CASE ELSE THEN stmt {$$ = make_mcase(Else,NULL,$4); }
    | CASE expr THEN stmt cases {($$ = make_mcase(Expr,$2,$4))->next = $5; }        

assign	: IDENT ASSIGN expr 
		{ $$ = make_stmt(Assign,NULL,make_ident($1),$3,NULL,NULL); }

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
		print_decl(proc->vars,1);
		print_stmt(proc->command,1);
		printf("\nend \n\n");
		print_lprocess(proc->next);
	};
	return 0;
}

// interpréteur aléatoire

void* link_vars_stmt(stmt* stmt,lprocess* p);


// le but de link_vars et de relier les variables dans les processus aux variables déclarées précédemment

void* link_vars_e(expr* e,lprocess* p)
{
	if (e) 
	{
		switch (e->type)
		{
			case Ident: return e->val.var = find_ident_p(e->val.var->name,p);
			break;
			case Xor:
			case Or:
			case And:
			case Not:
			case Gt:
			case Eq:
			case Plus:
			case Minus:
			case Times: 
				link_vars_e(e->val.sub.left,p); link_vars_e(e->val.sub.right,p);break;
			case Int: break;
		}
	}
}

void* link_vars_mc(mcase* cases,lprocess* p)
{
	if (cases)
	{
		switch (cases->type)
		{
			case Else : link_vars_stmt(cases->command,p);break;
			case Expr : link_vars_e(cases->cond,p);link_vars_stmt(cases->command,p);break;
		};
		link_vars_mc(cases->next,p);
	}
}


void* link_vars_stmt(stmt* stmt,lprocess* p)
{
	switch (stmt->type) 
	{
		case Assign : stmt->val.assign.var = find_ident_p(stmt->val.assign.var->name,p);  link_vars_e(stmt->val.assign.expr,p); break;
		case Semic : link_vars_stmt(stmt->val.sub.left,p);link_vars_stmt(stmt->val.sub.right,p);break;
		case Do : link_vars_mc(stmt->val.cases,p);break;
		case If : link_vars_mc(stmt->val.cases,p);break;
	}
}

void* link_vars_p (lprocess *p) 
{
	if (p) 
	{
		link_vars_stmt(p->command,p);
		link_vars_p(p->next);
	}
}

// renvoie la valeur d'une expression

int eval (expr *e)
{
	switch (e->type)
	{
		case Ident: return e->val.var->val;break;
		case Int: return e->val.i;break;
		case Xor: return eval(e->val.sub.left) ^ eval(e->val.sub.right);break;
		case Or: return eval(e->val.sub.left) || eval(e->val.sub.right);break;
		case And: return eval(e->val.sub.left) && eval(e->val.sub.right);break;
		case Not: return !eval(e->val.sub.left);break;
		case Gt: return eval(e->val.sub.left) > eval(e->val.sub.right);break;
		case Eq: return eval(e->val.sub.left) == eval(e->val.sub.right);break;
		case Plus: return eval(e->val.sub.left) + eval(e->val.sub.right);break;
		case Minus: return eval(e->val.sub.left) - eval(e->val.sub.right);break;
		case Times: return eval(e->val.sub.left) * eval(e->val.sub.right);break;
		case Zero: return 0;break;
	}
}

// INIT permet de mettre le 'youre_here' en début de processus

stmt* first_stmt(stmt *stmt) 
{
	switch (stmt->type) 
	{
		case Semic : first_stmt(stmt->val.sub.left); break;
		case Assign :
		case Do :
		case If :
		case Skip : 
		case Break : return stmt; break;
	}
}

void* init_stmt(stmt *stmt)
{
	switch (stmt->type) 
	{
		case Semic : init_stmt(stmt->val.sub.left); break;
		case Assign :
		case Do :
		case If :
		case Skip : 
		case Break : stmt->youre_here = 1; break;
	}
}

void* init_process(lprocess *p) 
{
	if (p) 
	{
		init_stmt(p->command);
		init_process(p->next);
	}
}

// renvoie le nombre de choix valide d'un mcase

int nb_choice(mcase* mc) 
{
	if (!mc) {return 0;}
	else
	{
		switch (mc->type)
		{
			case Else : return 0; break;
			case Expr : if (eval(mc->cond)) {return 1 + nb_choice(mc->next);} else {return nb_choice(mc->next);} break;
		}
	}
	
}

//renvoie le cas else d'un mcase

stmt* search_else(mcase *mc)
{
	if (mc) 
	{
		switch (mc->type)
		{
			case Else : return mc->command; break;
			case Expr : search_else(mc->next); break;
		}
	}
	else { return NULL ;}

}

// renvoie le kième choix valide d'une mcase

stmt* choose_nth(mcase *mc, int k)
{
	switch (mc->type)
	{
		case Else : break;
		case Expr : if (!k && eval(mc->cond)) {return mc->command;} else 
					{
						if (eval(mc->cond))
						{
							choose_nth(mc->next,k-1);
						}
						else 
						{
							choose_nth(mc->next,k);
						} 
					} break;
	}
}

// choisis aléatoirement une mcase valide

stmt* choose(mcase *mc) 
{
	int n = nb_choice(mc); 
	if (n) 
	{
		int k = rand() % n;
		return choose_nth(mc,k);
	}
	else 
	{
		return search_else(mc);
	}
}

void* execute_one_stmt(stmt* stmt, lprocess* p);
int is_finished(stmt* stmt); 
int is_at_end(stmt* stmt);

//is_finished vérifie que le 'youre_here' n'est pas ici

int is_finished_mc(mcase* mc) 
{
	if (!mc) {return 1;}
	else
	{
		return is_finished(mc->command) && is_finished_mc(mc->next);
	}
}

// is_at_end vérifie que le 'youre_here' est à la fin

int is_at_end_mc(mcase* mc) 
{
	if (!mc) {return 0;}
	else
	{
		return is_at_end(mc->command) || is_at_end_mc(mc->next);
	}
}

int is_finished(stmt* stmt) 
{
	switch (stmt->type)
	{
		case Assign : 
		case Skip :
		case Break : return (!stmt->youre_here); break;
		case Semic : return (is_finished(stmt->val.sub.left)) && (is_finished(stmt->val.sub.right)); break;
		case Do : 
		case If : return (is_finished_mc(stmt->val.cases)); break;
	}
}

int is_at_end(stmt* stmt)
{
	switch (stmt->type)
	{
		case Assign : 
		case Skip :
		case Break : return (stmt->youre_here); break;
		case Semic : if (is_finished(stmt->val.sub.left)) 
					{
						return is_at_end(stmt->val.sub.right);
					} break;
		case Do : 
		case If : return is_at_end_mc(stmt->val.cases);
	}
}

stmt* next_stmt(stmt* stmt);

//renvoie la commande qui suit la commande actuelle

stmt* next_stmt_mc(mcase* mc)
{
	if (mc) 
	{
		struct stmt* try1;
		try1 = next_stmt(mc->command);
		if (try1) {return try1;} else {next_stmt_mc(mc->next);}
	}
	else {return NULL;}
}


stmt* next_stmt(stmt* stmt)
{
	switch (stmt->type)
	{
		case Assign : return NULL; break;
		case Skip : return NULL; break;
		case Break : return NULL; break;
		case Semic : if (is_at_end(stmt->val.sub.left)) {return stmt->val.sub.right;}
					else 
					{
						struct stmt* first_try = next_stmt(stmt->val.sub.left);
						if (first_try) {return first_try;}
						else {
							struct stmt* second_try = next_stmt(stmt->val.sub.right);
							if (second_try) {return second_try;} else {return NULL;}
						}
					}
		case Do : if (stmt->youre_here) {return choose(stmt->val.cases);} 
				else 
				{
					if (is_at_end(stmt)) 
					{
						return stmt;
					}
					else 
					{
						return next_stmt_mc(stmt->val.cases);
					}
				} 
				break; 
		case If : if (stmt->youre_here) {return choose(stmt->val.cases);} else {return next_stmt_mc(stmt->val.cases);} break;
		
	}
}

stmt* next_stmt_p(lprocess* p)
{
	next_stmt(p->command);
}

void* execute_one_stmt(stmt* stmt, lprocess* p);

void* execute_one_case(mcase* mc, lprocess* p)
{
	if (mc)
	{
		execute_one_stmt(mc->command,p);
		execute_one_case(mc->next,p);
	}
}

// execute une commande dans le processus p

void* execute_one_stmt(stmt* stmt, lprocess* p)
{
	if (stmt) 
	{
		struct stmt* next;
		switch (stmt->type)
		{
			case Assign : 
				if (stmt->youre_here) 
				{
					stmt->val.assign.var->val = eval(stmt->val.assign.expr);
					next = first_stmt(next_stmt_p(p));
					if (next) 
					{
						stmt->youre_here = 0;
						next->youre_here = 1;
					}
					else 
					{
						p->alive = 0;
					}
				}
				break;
			case Semic :
				execute_one_stmt(stmt->val.sub.right,p); 
				execute_one_stmt(stmt->val.sub.left,p);
				break;
			case Do: 
				if (stmt->youre_here) 
				{
					next = first_stmt(next_stmt_p(p));
					stmt->youre_here = 0;
					next->youre_here = 1;
				}
				else 
				{
					execute_one_case(stmt->val.cases,p);
				}
				break;
			case If:
				if (stmt->youre_here) 
				{
					next = first_stmt(next_stmt_p(p));
					stmt->youre_here = 0;
					next->youre_here = 1;
				}
				else 
				{
					execute_one_case(stmt->val.cases,p);
				}
				break;
			case Skip:
				if (stmt->youre_here)
				{
					next = first_stmt(next_stmt_p(p));
					if (next) 
					{
						stmt->youre_here = 0;
						next->youre_here = 1;
					}
					else 
					{
						p->alive = 0;
					}
				} 
				break;
			case Break:
				if (stmt->youre_here)
				{
					next = first_stmt(next_stmt_p(p));
					if (next) 
					{
						stmt->youre_here = 0;
						next->youre_here = 1;
					}
					else 
					{
						p->alive = 0;
					}
				} 
				break;
		}
	}
}

void* execute_one_p(lprocess *p)
{
	execute_one_stmt(p->command,p);
}

int main (int argc, char **argv)
{
	srand(time(NULL));
	yyin = fopen(/*argv[1]*/ "./test2","r");
	yyparse();
	printf("parsing done \n");
	link_vars_p(process_list);
	decl_list->var->name = "a";
	init_process(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);
	printf("%d\n",decl_list->var->val);
	execute_one_p(process_list);

	
	
	

	printf("now printing \n");
	print_decl(decl_list,0);
	printf("\n");
	print_lprocess(process_list);
	printf("\n");
	print_specification(specification_list);
}
