%option noyywrap
%option yylineno

%x COMMENT
DIG	[0-9]


%%

"//" { BEGIN(COMMENT); }
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

{DIG}+     { yylval.e = make_expr(Int,atoi(yytext),NULL,NULL,NULL); return INT; }

";"    { return SEMC ;}
","    { return COMMA ; }
"::"   { return CASE ; }
"->"    { return THEN ; }
">"    { return  GT ; }
"=="    { return EQ ; }
":="    { return ASSIGN ; }
"^"     { return XOR ; }
"||"    { return OR; }
"&&"    { return AND; }
"!"    { return NOT; }
"+"    { return PLUS ; }
"-"    { return MINUS ; }
"*"    { return TIMES ; }

<COMMENT>. { }
<COMMENT>\n { BEGIN(INITIAL); }

[a-z][a-z0-9]* { yylval.c = strdup(yytext); return IDENT; }
"//"[a-zA-Z0-9é ]*"\n" {  }
[	 \n]    { }

.	{ }

%%
