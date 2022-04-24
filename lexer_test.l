%option noyywrap

DIG	[0-9]
%%

"var"    { printf("VAR "); }
"proc"    { printf("PROC "); }
"do"    { printf("DO "); }
"od"    { printf("OD "); }
"if"    { printf("IF "); }
"fi"    { printf("FI "); }
"else"    { printf("ELSE "); }
"skip"    { printf("SKIP "); }
"end"    { printf("END "); }
"reach"    { printf("REACH "); }
"break"    { printf("BREAK "); }

{DIG}+     {printf("INT "); }

";" {printf("SEMC "); }
"," {printf("COMMA "); }
"::"   { printf("CASE  "); }
"->"    { printf("THEN  "); }
">"    { printf(" GT  "); }
"=="    { printf("EQ  "); }
":="    { printf("ASSIGN  "); }
"^"     { printf("XOR  "); }
"||"    { printf("OR "); }
"&&"    { printf("AND "); }
"!"    { printf("NOT "); }
"+"    { printf("PLUS  "); }
"-"    { printf("MINUS  "); }
"*"    { printf("TIMES  "); }

[a-z][a-z0-9]* {printf("IDENT "); }
"//"[a-z0-9A-Z ]*"\n" { }
[	 \n]    { }

.	{ printf("erreur"); }

%%

int main (int argc, char **argv)
{
	if (argc > 1) yyin = fopen(argv[1],"r ");
	yylex();
	puts(" ");
}
