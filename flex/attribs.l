%option noyywrap

%{
enum { T_INT = 1, T_CHAR = 2 };
union {
	int i;
	char c;
} data; 
%}

DIG	[0-9]

%%

{DIG}+	{ data.i = atoi(yytext); return T_INT; }
[+*()]	{ data.c = *yytext; return T_CHAR; }
[ \n]   { /* ignore */ }

%%

int main (int argc, char **argv)
{
	int token;
	if (argc > 1) yyin = fopen(argv[1],"r");
	while (token = yylex())
	{
		switch(token)
		{
		  	case T_INT: printf("int(%d)  ",data.i); break;
		  	case T_CHAR: printf("'%c'  ",data.c); break;
		}
	}
	puts("");
}
