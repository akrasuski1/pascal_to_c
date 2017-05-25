%{
  #include <stdio.h>
  int yywrap(void);
  int yylex(void);
  void yyerror(const char *fmt, ...);
  #include "y.tab.h"
  enum { BODY_LEVEL, INSIDE_SLC, INSIDE_OSC, INSIDE_NSC } inside_comment = BODY_LEVEL;
  int indent = 2;
%}
%s SLC OSC NSC ARR
%%
<INITIAL>[pP][rR][oO][gG][rR][aA][mM]       return KWD_PROGRAM;
<INITIAL>[bB][eE][gG][iI][nN]               return KWD_BEGIN;
<INITIAL>[eE][nN][dD]                       return KWD_END;
<INITIAL>[vV][aA][rR]                       return KWD_VAR;
<INITIAL>[cC][oO][nN][sS][tT]               return KWD_CONST;
<INITIAL>[rR][eE][cC][oO][rR][dD]           return KWD_RECORD;
<INITIAL>[aA][rR][rR][aA][yY]				{ BEGIN ARR; return KWD_ARRAY; }

<INITIAL>\.                                 return '.';
<INITIAL>\(                                 return '(';
<INITIAL>\)                                 return ')';
<INITIAL>\,                                 return ',';
<INITIAL>\;                                 return ';';
<INITIAL>\:                                 return ':';
<INITIAL>\=                                 return '=';

<INITIAL>:=				return TOK_ASSIGN;

<INITIAL>[nN][oO][tT]	return TOK_NOT;
<INITIAL>\@				return '@';

<INITIAL>\*\*			return TOK_POW;

<INITIAL>\*				return '*';
<INITIAL>\/				return '/';
<INITIAL>[dD][iI][vV]	return TOK_DIV;
<INITIAL>[mM][oO][dD]	return TOK_MOD;
<INITIAL>[aA][nN][dD]	return TOK_AND;
<INITIAL>[sS][hH][lL]	return TOK_SHL;
<INITIAL>[sS][hH][rR]	return TOK_SHR;
<INITIAL>[aA][sS]		return TOK_AS;
<INITIAL><<				return TOK_SHL;
<INITIAL>>>				return TOK_SHR;

<INITIAL>\+				return '+';
<INITIAL>-				return '-';

<INITIAL>[oO][rR]		return TOK_OR;
<INITIAL>[xX][oO][rR]	return TOK_XOR;

<INITIAL>[iI][nN]		return TOK_IN;
<INITIAL>[iI][sS]		return TOK_ISS;
<INITIAL><>				return TOK_NEQ;
<INITIAL><				return '<';
<INITIAL>>				return '>';
<INITIAL><=				return TOK_LE;
<INITIAL>>=				return TOK_GE;

<INITIAL>[_a-zA-Z][_0-9a-zA-Z]*             { yylval.text = strdup(yytext);
                                              return IDENT;
                                            }


<INITIAL,ARR>\'[^']+\'                    {
                                              yylval.text = strdup(yytext);
					      return CHAR_CONST;
					    }
<INITIAL,ARR>[+-]?[0-9]+                        { 
                                              yylval.text = strdup(yytext);
					      return INT_CONST;
					    }
<INITIAL>[-+]?[0-9]+(\.?[0-9]*)?([eE][+-]?[0-9]+)?    {
                                              yylval.text = strdup(yytext);
					      return REAL_CONST;
					    }
<INITIAL,ARR>\[                                 return '[';
<INITIAL,ARR>"(."                               return '[';

<INITIAL,ARR>\]                                 return ']';
<INITIAL,ARR>".)"                               return ']';

<INITIAL>"//"                               { printf("//"); inside_comment = INSIDE_SLC; BEGIN SLC; } /* single line comment */
<INITIAL>"(*"                               { printf("/*"); inside_comment = INSIDE_OSC; BEGIN OSC; } /* old style comment */
<INITIAL>\{                                 { printf("/*"); inside_comment = INSIDE_NSC; BEGIN NSC; } /* new style comment */

<INITIAL>\}                                 { yyerror("\nUnexpected end of comment '}'");  exit(-1); }
<INITIAL>"*)"                               { yyerror("\nUnexpected end of comment '*)'"); exit(-1); }

<INITIAL,ARR>" "|\t|\v|\n              ;

<SLC>\n                            { ECHO; inside_comment = BODY_LEVEL; BEGIN INITIAL; }
<SLC>"/*"                          printf("/ *");
<SLC>"*/"                          printf("* /");
<SLC>.                             ECHO;

<OSC>"*)"                          { puts("*/"); inside_comment = BODY_LEVEL; BEGIN INITIAL; }
<OSC>"/*"                          printf("/ *");
<OSC>"*/"                          printf("* /");
<OSC>\n                            ECHO;
<OSC>.                             ECHO;

<NSC>"}"                           { puts("*/"); inside_comment = BODY_LEVEL; BEGIN INITIAL; }
<NSC>"/*"                          printf("/ *");
<NSC>"*/"                          printf("* /");
<NSC>\n                            ECHO;
<NSC>.                             ECHO;

<ARR>".."           return TOK_DOTDOT;
<ARR>","            return ',';
<ARR>[oO][fF]       { BEGIN INITIAL; return KWD_OF; }




%%
int yywrap(void)
{
  switch(inside_comment)
  {
    case INSIDE_SLC: yyerror("\nUnexpected end of file inside line comment '//'");
                     exit(-1);
    case INSIDE_OSC: yyerror("\nUnexpected end of file inside old style comment '(* *)'");
                     exit(-1);
    case INSIDE_NSC: yyerror("\nUnexpected end of file inside comment '{ }'");
                     exit(-1);
  }
  return 1;
}
