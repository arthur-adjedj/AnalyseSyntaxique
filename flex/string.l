%option noyywrap

DIG	[0-9]

%%

{DIG}+	{ printf("int(%d)  ",atoi(yytext)); }
[+*()]	{ printf("'%c'  ",*yytext); }
[ \n]   { /* ignore */ }

%%

int main (int argc, char **argv)
{
	YY_BUFFER_STATE buffer;

	buffer = yy_scan_string("100+5*2");
	yylex();
	yy_delete_buffer(buffer);

	buffer = yy_scan_string("20+30");
	yylex();
	yy_delete_buffer(buffer);

	puts("");
}
