/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tfpager.c,v $
 *	$Author: ghudson $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tfpager.c,v 1.2 1996-09-19 22:30:53 ghudson Exp $
 *
 *	Copyright (C) 1987 by the Massachusetts Institute of Technology
 *
 *	An auto-paging tfile.
 *
 *	$Log: not supported by cvs2svn $
 *	Revision 1.1  1993/10/12 05:58:26  probe
 *	Initial revision
 *
 */

#ifndef lint
static char *rcsid_tfpager_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tfpager.c,v 1.2 1996-09-19 22:30:53 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include "tfile.h"
#include <ctype.h>
#define TABSTOP	8

#define FLUSHMSG 1
#define FLUSHSEQ 2
#define NO_TERMCAP_FILE	3
#define UNKNOWN_TT 4

char *clear_eos;
char *clear_eol;
int auto_wrap;
char *standout, *standin, *home_cursor;
int xsize, ysize;	/* X, Y size of terminal */
char buf[1024];
char *bp;

extern int errno;
extern char *getenv();
extern char *tgetstr();
int tgetent();
int tgetnum();

typedef struct tpager_info {
	int curln, curcol;	/* current line, current column */
	int tsize;		/* total size of data */
	int sofar;		/* amount seen so far */
	int errno;
	u_int counting:1;		/* are we counting data sent to us? */
	u_int clear_on_open:1;
	u_int home_on_page:1;
	u_int clear_on_page:1;
} tpager_info;

void set_no_echo()
{
	struct sgttyb sgb;
	ioctl(fileno(stdin), TIOCGETP, &sgb);
	sgb.sg_flags |= CBREAK;
	sgb.sg_flags &= ~ECHO;
	ioctl(fileno(stdin), TIOCSETN, &sgb);
}

void set_echo()
{
	struct sgttyb sgb;
	ioctl(fileno(stdin), TIOCGETP, &sgb);
	sgb.sg_flags &= ~CBREAK;
	sgb.sg_flags |= ECHO;
	ioctl(fileno(stdin), TIOCSETN, &sgb);
}

/*ARGSUSED*/
tpager (op, pgr_p, info, argp, argn, result)
	int op, argn, *result, *info;
	tpager_info **pgr_p;
	char *argp;
{
	char *cp;
	int count;
	tpager_info *pgr = *pgr_p;
	*result = 0;
	
	switch(op) {
	case TFOPEN:
		if (pgr->clear_on_open) {
			fputs(home_cursor, stdout);
			fputs(clear_eos, stdout);
		}
		set_no_echo();
		break;
		
	case TFCLOSE:
		set_echo();
		break;

	case TFREAD:
		*result = EINVAL;
		return(-1);
		break;
		
	case TFWRITE:
#define checkpwrap(pgr) if (pgr->curln > ysize) { \
				pgr_prompt(pgr);	\
				if(pgr->errno) break;   \
			}

#define checklwrap(pgr) if (pgr->curcol > xsize) { \
				pgr->curcol = 0;	\
			        if (auto_wrap)	\
					putc('\n', stdout); \
				if (pgr->home_on_page)	\
					fputs(clear_eol, stdout); \
				pgr->curln++;		\
				checkpwrap(pgr);	\
			}

		for (cp = argp, count=argn; count; cp++, count--) {
			register unsigned char c = toascii(*cp);
			if (pgr->counting) pgr->sofar++;
			if(isprint(c)) {
				putc(c, stdout);
				pgr->curcol++;
				checklwrap(pgr);
			} else if (c == '\n') {
				putc(c, stdout);
				if (pgr->home_on_page)
					fputs(clear_eol, stdout);
				pgr->curcol = 0;
				pgr->curln++;
				checkpwrap(pgr);
			} else if (c == '\t') {
				do {
					putc(' ', stdout);
					pgr->curcol++;
					checklwrap(pgr);
				} while(pgr->curcol % TABSTOP);
			} else {
				putc('^', stdout);
				pgr->curcol++;
				checklwrap(pgr);
				putc(c+'@', stdout);
				pgr->curcol++;
				checklwrap(pgr);
			}
		}				
		break;
	}		
	*result = pgr->errno;
}

pgr_prompt(pgr)
	tpager_info *pgr;
{
	char in_c;
	
	/* this only gets invoked at the start of a line */
	fputs(standout,stdout);
	fputs("--More--", stdout);
	fprintf(stdout, "(%d%%)", (pgr->sofar * 100) * pgr->tsize);
	fputs(standin,stdout);
	fflush(stdout);

	do {
	loop:
		in_c = getchar();
		switch(in_c) {
		case '\r':
		case '\n':
			pgr->curln--;
			break;
		case ' ':
			pgr->curln=0;
			break;
		case 'n':
			pgr->errno = FLUSHMSG;
			break;
		case 'q':
			pgr->errno = FLUSHSEQ;
			break;
		default:
			putc('\7', stdout);
			fflush(stdout);
			goto loop;
		}
	} while(0);
	
	pgr->curcol=0;

	putc('\r',stdout);
	if (clear_eol)
		fputs(clear_eol, stdout);
	else fputs("        \r", stdout);
	if (!pgr->curln) {
		if (pgr->home_on_page || pgr->clear_on_page)
			fputs(home_cursor, stdout);
		if (pgr->clear_on_page)
			fputs(clear_eos, stdout);
	} 
}	
/*
 * Deal with window size change signal (turn the winch... )
 */

crank()
{
#ifdef TIOCGWINSZ
	struct winsize ws;

	if (ioctl(fileno(stdout), TIOCGWINSZ, &ws) < 0) {
#endif TIOCGWINSZ
		ysize = tgetnum("li");
		xsize = tgetnum("co");
#ifdef TIOCGWINSZ
	} else {
		if ((ysize = ws.ws_row) == 0)
			ysize = tgetnum("li");
		if ((xsize = ws.ws_col) == 0)
			xsize = tgetnum("co");
	}
#endif TIOCGWINSZ
	ysize--;
}       
int init_tfpager()
{
	char *tt;
	int code;
	
	if (!isatty(fileno(stdout))) {
	punt:
		ysize = 100000;
		xsize = 100000;
		errno = 0;
		return(0);
	}

	if (!(tt = getenv("TERM")))
		goto punt;
	
	code = tgetent(buf, tt);
	if (code == -1) code = NO_TERMCAP_FILE;
	else if (code == 0) code = UNKNOWN_TT;
	else {
		bp = buf;
		clear_eos = tgetstr("cd", &bp);
		clear_eol = tgetstr("ce", &bp);
		standout = tgetstr("so", &bp);
		standin = tgetstr("se", &bp);
		home_cursor = tgetstr("ho", &bp);
		auto_wrap = tgetflag("am");
#ifdef SIGWINCH
		signal(SIGWINCH, crank);		
#endif SIGWINCH
		crank();
		code = 0;
	} 
	return code;
} 

tfile pager_tfile()
{
	tpager_info *pgr = (tpager_info *)malloc(sizeof(*pgr));
	memset(pgr, 0, sizeof(*pgr));
	pgr->counting = 1;
	pgr->clear_on_open = 1;
	pgr->clear_on_page = 1;
	printf("pgr = %x\n", pgr);
	return(tcreate(0, (char *)pgr, 0, tpager));
} 
