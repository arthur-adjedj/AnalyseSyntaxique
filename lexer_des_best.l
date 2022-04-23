%option noyywrap

DIG	[0-9]

%%

"var"    { return VAR; }
"proc"    { return PROC; }
"do"    { return DO; }
"od"    { return OD; }
"if"    { return IF; }
"fi"    { return FI; }
"else"    { return ELSE; }
"skip"    { return SKIP; }
"end"    { return END; }
"reach"    { return REACH; }
"break"    { return BREAK; }

{DIG}+     { yylval.i = atoi(yytext); return INT; }

","     { return COMMA ; }
";"     { return SEMC ; }
">"    { return  GT}
"=="    { return EQ; }
":="    { return ASSIGN; }
"^"        { return XOR; }
"||"    { return OR; }
"&&"    { return AND; }
"!"    { return NOT; }
"+"    { return PLUS ; }
"-"    { return MINUS ; }
"*"    { return TIMES ; }

[a-z][a-z0-9]* { yylval.c = strdup(yytext); return IDENT; }

[ \n]   {  }

.	{ printf("nique ta mère") ; }

%%
