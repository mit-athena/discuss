%token INTEGER NREF PREF FIRST LAST NEXT PREV AREF LREF CUR
%left '+' '-'
%{
#define TRNERR_SYNERR 1
#include <ctype.h>
#include "interface.h"

static trn_info *trnexpr_curtrn;
static mtg_info *trnexpr_curmtg;
static int trnexpr_low, trnexpr_high;
static char *cp;
static int trnexpr_err;
%}

%%
range	: trn_no 
		{ trnexpr_low = $1;
		   trnexpr_high = -1;
		}
	| trn_no ':' trn_no
		{ trnexpr_low = $1;
		  trnexpr_high = $3;
		}
	;

trn_no	: INTEGER
	| NREF
	| PREF
	| FIRST
	| LAST
	| NEXT
	| PREV
	| AREF
	| LREF
	| CUR
	| expr
	;

expr	: trn_no '-' trn_no
		{ $$ = $1 - $3; }
	| trn_no '+' trn_no
		{ $$ = $1 + $3; }
	;
%%

static yyerror() 
{
	trnexpr_err = TRNERR_SYNERR;
}

yylex() 
{	
	if (!*cp) return -1;
	if(isdigit(*cp)) {
		yylval=0;
		do {
			yylval = *(cp++) - '0' + yylval*10;
		} while (*cp && isdigit(*cp));
		return( INTEGER );
	} else if (*cp=='.') {
		cp++;
		yylval=trnexpr_curtrn->current;
		return(CUR);
	} else if (*cp==':' || *cp=='+' || *cp=='-') {
		return(*cp++);
	} else if (!strncmp(cp, "next", 4)) {
		cp += 4;
		yylval=trnexpr_curtrn->next;
		return(NEXT);
	} else if (!strncmp(cp, "prev", 4)) {
		cp += 4;
		yylval=trnexpr_curtrn->prev;
		return(PREV);
 	} else if (!strncmp(cp, "nref", 4)) {
		cp += 4;
		yylval=trnexpr_curtrn->nref;
		return(NREF);
	} else if (!strncmp(cp, "pref", 4)) {
		cp += 4;
		yylval=trnexpr_curtrn->pref;
		return(PREF);
 	} else if (!strncmp(cp, "first", 5)) {
		cp += 5;
		yylval=trnexpr_curmtg->first;
		return(FIRST);
	} else if (!strncmp(cp, "last", 4)) {
		cp += 4;
		yylval=trnexpr_curmtg->last;
		return(LAST);
	} else if (*cp=='n') {
		cp++;
		yylval=trnexpr_curtrn->next;
		return(NEXT);
	} else if (*cp=='p') {
		cp++;
		yylval=trnexpr_curtrn->prev;
		return(PREV);
  	} else if (*cp=='l') {
		cp++;
		yylval=trnexpr_curmtg->last;
		return(LAST);
 	} else if (*cp=='f') {
		cp++;
		yylval=trnexpr_curmtg->first;
		return(FIRST);
	} else return (*cp++);
}

int trnexpr_parse(mtg, trn, string, lorange, highrange) 
        mtg_info *mtg;
  	trn_info *trn;
	char *string;
	int *lorange, *highrange;
{
	cp = string;
	trnexpr_curmtg = mtg;
	trnexpr_curtrn = trn;
	trnexpr_err = 0;
	yyparse();
	if(lorange)*lorange =trnexpr_low;
	if(highrange)*highrange = trnexpr_high;
	return(trnexpr_err);
}

#ifdef notdef
main(argc, argv) 
	int argc;
	char **argv;
{
	int low, high;
	argv++; argc--;
	while(argc--) {
		low = -1; high = -1;
		trnexpr_parse(*argv, &low, &high);
		printf("%s: %d:%d\n", *argv, low, high);
		if(trnexpr_err) {
			printf("Error %d\n", trnexpr_err);
			trnexpr_err=0;
		}
		argv++;
	}
}


#endif notdef
