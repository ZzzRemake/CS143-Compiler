/*  
 *  注意改名为cool.flex
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1024
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

int comment_layer=0;

extern int curr_lineno; // 维护的line no。
extern int verbose_flag;

extern YYSTYPE cool_yylval; // 返回的yylval

/*
 *  Add Your own definitions here
 */

%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */

DARROW          =>
INTEGER         [0-9]+
IDENTIFIERS     [a-z_][a-zA-Z0-9_]*


%Start COMMENT
%Start INLINE_COMMENT
%Start STRING

%%

 /*
  *  Nested comments
  */


<INITIAL,COMMENT,INLINE_COMMENT>"(*" {
  comment_layer++;
  BEGIN(COMMENT);
}

<COMMENT>{
  "*)"      {
    if(--comment_layer == 0)
      BEGIN(INITIAL);
  }
  [^(*\n]+   {  }
  [()*]     {  }
  <<EOF>>   {
    cool_yylval.error_msg = "EOF in comment";
    BEGIN(INITIAL);
    comment_layer = 0;
    return ERROR;
  }
}

<INITIAL>"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return ERROR;
}

<INITIAL>"--" {
  BEGIN(INLINE_COMMENT);
}

<INLINE_COMMENT>{
  \n {
    curr_lineno++;
    BEGIN(INITIAL);
  }
  [^\n]* {  }

}

<INITIAL>\" {
  BEGIN(STRING);
}

<STRING>{
  \" {
    std::string text(yytext, yyleng-1);

    if(text.find('\0') != std::string::npos){
      cool_yylval.error_msg = "String contains null character";
      BEGIN(INITIAL);
      return ERROR;
    }

    std::string out;
    bool flag=false;
    char temp_c;
    for(auto c: text){
      if(flag){
        switch (c) {
          case 'b':
            temp_c = '\b'; break;
          case 't':
            temp_c = '\t'; break;
          case 'n':
            temp_c = '\n'; break;
          case 'f':
            temp_c = '\f'; break;
          case '0':
            temp_c = '\0'; break;
          default:
            temp_c = c; break;
        }
        flag = false;
      } else {
        temp_c = c;
      }
      if(c == '\\'){
        flag = true;
      }
      if(!flag){
        out.push_back(temp_c);
      }
    }
    
    if(out.size() > MAX_STR_CONST){
      cool_yylval.error_msg = "String constant too long";
      BEGIN(INITIAL);
      return ERROR; 
    }

    cool_yylval.symbol = stringtable.add_string((char*)out.c_str());
    BEGIN(INITIAL);
    return STR_CONST;
  }
  \n {
    cool_yylval.error_msg = "EOF in string constant";
    curr_lineno++;
    BEGIN(INITIAL);
    return ERROR;
  }
  \\0 {
    cool_yylval.error_msg = "String contains null character";
    BEGIN(INITIAL);
    return ERROR;
  }
  <<EOF>> {
    cool_yylval.error_msg = "EOF in string constant";
    BEGIN(INITIAL);
    return ERROR;
  }
  \\[^\n]     { yymore(); }
  [^\\\"\n*]* { yymore(); }
}

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

(?i:class)  { return (CLASS); }

(?i:else)   { return (ELSE); }

(?i:fi)     { return (FI); }

(?i:if)     { return (IF); }

(?i:in)     { return (IN); }

(?i:inherits)     { return (INHERITS); }

(?i:let)     { return (LET); }

(?i:loop)     { return (LOOP); }

(?i:pool)     { return (POOL); }

(?i:THEN)     { return (THEN); }

(?i:WHILE)     { return (WHILE); }

(?i:CASE)     { return (CASE); }

(?i:ESAC)     { return (ESAC); }

(?i:OF)       { return (OF); }

(?i:NEW)     { return (NEW); }

(?i:ISVOID)     { return (ISVOID); }

(?i:NOT)      { return (NOT); }

t(?i:rue)    {
  cool_yylval.boolean = 1;
  return BOOL_CONST;
}

f(?i:alse)   {
  cool_yylval.boolean = 0;
  return BOOL_CONST;
}

[ \t\r\f\v]+  {  }


\n            { curr_lineno++; }

{INTEGER}     {
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}

{IDENTIFIERS} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

[A-Z][a-zA-Z0-0_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

"<-"          { return ASSIGN; }

"<="          { return LE; }

"<"           { return int('<'); }

"="           { return int('='); }

"+"           { return int('+'); }

"-"           { return int('-'); }

","           { return int(','); }

";"           { return int(';'); }

"("           { return int('('); }

")"           { return int(')'); }

"{"           { return int('{'); }

"}"           { return int('}'); }

":"           { return int(':'); }

"~"           { return int('~'); }

"*"           { return int('*'); }

"/"           { return int('/'); }

"@"           { return int('@'); }

"."           { return int('.'); }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

[^\n]   {
  cool_yylval.error_msg = yytext;
  return ERROR;
}

%%
