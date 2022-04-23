%option noyywrap

DIG	[0-9]
VAR [a-zA-Z]|[_]
SYM [:=<>+-*/&|]

%%

{DIG}+	{ printf("int(%d)  ",atoi(yytext)); }
{VAR}+  { printf("var(%d)  ",atoi(yytext)); }
[+*()]	{ printf("'%c'  ",*yytext); }
"if"    { printf(" IF "); }
"then"  { printf(" THEN "); }
"else"  { printf(" ELSE "); }
[ \n]   { /* ignore */ }
. {}

%%

int main (int argc, char **argv)
{
	if (argc > 1) yyin = fopen(argv[1],"r");
	yylex();
	puts("");
}
