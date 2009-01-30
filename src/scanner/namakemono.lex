/* flex sample.lex; gcc lex.yy.c -lfl */

%{
	#define CHAR_MAX 1024

	int last_state = -1;

	char buffer[CHAR_MAX];
	int buffer_index = 0;

	void clear_buffer(){
		memset(buffer, 0, sizeof(buffer));
		buffer_index = 0;
	}

	void add_to_buffer(char c){
		buffer[buffer_index++] = c;
	}

	int get_pipe_pos(char* str){
		int i = 0, j = 0, len = 0;
		char tmp[CHAR_MAX];

		len = strlen(str);
		for(i = 1, j = 0; i < len; ++i, ++j){
			tmp[j] = str[i];
		}
		tmp[j] = '\0';
		return atoi(tmp);
	}
%}

DIGIT 	[0-9]
CHAR 	[A-Za-z_\+\-\*\/\%\=\<\>\!\?]

%x S_STRING
%x D_STRING
%x REGEXP
%x ESCAPE
%x ONE_LINE_COMMENT
%x MULTI_LINE_COMMENT

%%
"#|" {
	BEGIN(MULTI_LINE_COMMENT);
}
<MULTI_LINE_COMMENT>. {}
<MULTI_LINE_COMMENT>"|#" {
	BEGIN(INITIAL);
}

";" {
	BEGIN(ONE_LINE_COMMENT);
}
<ONE_LINE_COMMENT>. {}
<ONE_LINE_COMMENT>\n {
	BEGIN(INITIAL);
}

"#!" {
	BEGIN(ONE_LINE_COMMENT);
}

"#/" {
	clear_buffer();
	BEGIN(REGEXP);
}
<REGEXP>[^\/\\] {
	add_to_buffer(yytext[0]);
}
<REGEXP>"\\" {
	add_to_buffer(yytext[0]);
	last_state = REGEXP;
	BEGIN(ESCAPE);
}
<REGEXP>"\/" {
	add_to_buffer('\0');
	BEGIN(INITIAL);
	printf("(:regexp #/%s/) ", buffer);
}

[\r\n]+ {
	/* =end */
	printf("(:end '()) ");
}

"\"" {
	clear_buffer();
	BEGIN(D_STRING);
}
<D_STRING>[^\"\\] {
	add_to_buffer(yytext[0]);
}
<D_STRING>"\\" {
	add_to_buffer(yytext[0]);
	last_state = D_STRING;
	BEGIN(ESCAPE);
}
<D_STRING>"\"" {
	add_to_buffer('\0');
	BEGIN(INITIAL);
	printf("(:string \"%s\") ", buffer);
}

"'" {
	clear_buffer();
	BEGIN(S_STRING);
}
<S_STRING>[^'\\] {
	add_to_buffer(yytext[0]);
}
<S_STRING>"\\" {
	add_to_buffer(yytext[0]);
	last_state = S_STRING;
	BEGIN(ESCAPE);
}
<S_STRING>"'" {
	add_to_buffer('\0');
	BEGIN(INITIAL);
	printf("(:string \"%s\") ", buffer);
}

<ESCAPE>. {
	add_to_buffer(yytext[0]);
	if(last_state != -1){
		BEGIN(last_state);
	} else {
		BEGIN(INITIAL);
	}
}

":"{CHAR}+ {
	/* =keyword */
	printf("(:keyword %s) ", yytext);
}

"[" {
	/* =lambda start */
	printf("(:lambda-start '()) ");
}

"]" {
	/* =lambda end */
	printf("(:lambda-end '()) ");
}

"(" {
	/* =parameter start */
	printf("(:parameter-start '()) ");
}

")" {
	/* =parameter end */
	printf("(:parameter-end '()) ");
}

"."[0-9]+"." {
	/* =pipe-pos */
	printf("(:pipe '()) ");
	printf("(:pipe-pos %d) ", get_pipe_pos(yytext));
}

"." {
	/* =pipe */
	printf("(:pipe '()) ");
}

"this" {
	/* =this */
	printf("(:this '())");
}

"-"?{DIGIT}+(("."{DIGIT}+)?|"/"{DIGIT}+) {
	/* =number */
	printf("(:number %s) ", yytext);
}

{CHAR}+({DIGIT}+({CHAR}+)?)? {
	/* =word */
	printf("(:word :%s) ", yytext);
}

[\ \t] { /* space */ }

. {
	printf("scanning error: unknown token = '%s'\n", yytext);
	exit(-1);
}


%%

main(argc, argv)
int argc;
char **argv;
{
	++argv, --argc;
	if(argc > 0){
		yyin = fopen(argv[0], "r");
	} else {
		yyin = stdin;
	}
	printf("(");
	yylex();
	printf(")");

	return 0;
}
