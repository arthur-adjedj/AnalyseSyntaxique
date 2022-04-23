%option noyywrap

DIG	[0-9]

%%

{DIG}+	{ yylval.i = atoi(yytext); return INT; }
[+*()]	{ return *yytext; }
[ \n]   { /* ignore */ }

%%

