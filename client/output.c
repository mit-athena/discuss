/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.14 1996-09-08 20:31:27 ghudson Exp $
 *	$Locker:  $
 *
 *	Utility routines.
 *
 */

#ifndef lint
static char rcsid_discuss_utils_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.14 1996-09-08 20:31:27 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <signal.h>
#include <ss/ss.h>
#include <discuss/discuss.h>
#include "config.h"
#include "globals.h"

extern ss_request_table discuss_cmds;
extern char *temp_file;
extern char *pgm;
extern char *malloc(), *getenv(), *short_time();

output_trans(tinfop, tf, code)
	trn_info3 *tinfop;
	tfile tf;
	int *code;
{
	char *plural;
	char newtime[26];
	char line[255];
	int flagged;

	if (*code != 0) return;

	(void) strcpy (newtime, short_time (&tinfop->date_entered));
	newtime [24] = '\0';			/* get rid of \n */

	if (tinfop->num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	if (tinfop -> signature != NULL && *(tinfop -> signature) != '\0' &&
	     strcmp(tinfop -> signature, tinfop->author)) {
	     (void) sprintf (line, "[%04d] %s (%s)  %s  %s (%d line%s)\n",
			     tinfop->current, tinfop->author,
			      tinfop->signature, dsc_public.m_info.long_name,
			     newtime, tinfop->num_lines, plural);
	} else {
	     (void) sprintf (line, "[%04d] %s  %s  %s (%d line%s)\n",
			     tinfop->current, tinfop->author,
			     dsc_public.m_info.long_name,
			     newtime, tinfop->num_lines, plural);
	}
	twrite (tf, line, strlen (line), code);
	if (tinfop->subject [0] != '\0') {
		twrite (tf, "Subject: ", 9, code);
		twrite (tf, tinfop->subject, strlen (tinfop->subject), code);
		twrite (tf, "\n", 1, code);
	}
	dsc_get_trn(&dsc_public.nb, tinfop->current, tf, code);
	if (*code != 0) return;

	/* Force a NL in case the transaction doesn't have one.
	   Tfile's now have Control operations that allow us to
	   do this */
	tcontrol(tf, TFC_FORCE_NL, 0, code);

	flagged = (tinfop->flags & TRN_FLAG1) != 0;
	if (tinfop->pref == 0 && tinfop->nref == 0)
		(void) sprintf (line, "--[%04d]--%s\n\f\n", tinfop->current,
				flagged ? " (flagged)" : "");
	else if (tinfop->pref == 0)
		(void) sprintf (line, "--[%04d]-- (nref = [%04d]%s)\n\f\n",
				tinfop->current, tinfop->nref,
				flagged ? ", flagged" : "");
	else if (tinfop->nref == 0)
		(void) sprintf (line, "--[%04d]-- (pref = [%04d]%s)\n\f\n",
				tinfop->current, tinfop->pref,
				flagged ? ", flagged" : "");
	else
	     (void) sprintf (line,
			     "--[%04d]-- (pref = [%04d], nref = [%04d]%s)\n\f\n",
			     tinfop->current, tinfop->pref, tinfop->nref,
			     flagged ? ", flagged" : "");
	twrite (tf, line, strlen (line), code);
}
