/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.9 1989-01-29 17:08:58 srz Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986, 1988 by the Student Information Processing Board.
 *	
 *	Utility routines.
 *
 */

#ifndef lint
static char rcsid_discuss_utils_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.9 1989-01-29 17:08:58 srz Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <signal.h>
#include "ss.h"
#include <discuss/discuss.h>
#include "config.h"
#include "globals.h"

extern ss_request_table discuss_cmds;
extern char *temp_file;
extern char *pgm;
extern char *malloc(), *getenv(), *short_time();

output_trans(txn_no, tf, code)
	trn_nums txn_no;
	tfile tf;
	int *code;
{
	char *plural;
	char newtime[26];
	char line[255];
	trn_info2 tinfo;

	*code = 0;
	dsc_get_trn_info2(&dsc_public.nb, txn_no,
			 &tinfo, code);
	if (*code != 0) return;

	(void) strcpy (newtime, short_time (&tinfo.date_entered));
	newtime [24] = '\0';			/* get rid of \n */

	if (tinfo.num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	(void) sprintf (line, "[%04d] %s  %s  %s (%d line%s)%s\n",
			tinfo.current, tinfo.author,
			dsc_public.m_info.long_name,
			newtime, tinfo.num_lines, plural,
			((tinfo.flags & TRN_FLAG1) != 0) ? " (flagged)" : "");
	twrite (tf, line, strlen (line), code);
	if (tinfo.subject [0] != '\0') {
		twrite (tf, "Subject: ", 9, code);
		twrite (tf, tinfo.subject, strlen (tinfo.subject), code);
		twrite (tf, "\n", 1, code);
	}
	dsc_get_trn(&dsc_public.nb, txn_no, tf, code);
	if (*code != 0) return;

	/* Force a NL in case the transaction doesn't have one.
	   Tfile's now have Control operations that allow us to
	   do this */
	tcontrol(tf, TFC_FORCE_NL, 0, code);

	if (tinfo.pref == 0 && tinfo.nref == 0)
		(void) sprintf (line, "--[%04d]--\n\f\n", tinfo.current);
	else if (tinfo.pref == 0)
		(void) sprintf (line, "--[%04d]-- (nref = [%04d])\n\f\n",
				tinfo.current, tinfo.nref);
	else if (tinfo.nref == 0)
		(void) sprintf (line, "--[%04d]-- (pref = [%04d])\n\f\n",
				tinfo.current, tinfo.pref);
	else
	     (void) sprintf (line,
			     "--[%04d]-- (pref = [%04d], nref = [%04d])\n\f\n",
			     tinfo.current, tinfo.pref, tinfo.nref);
	twrite (tf, line, strlen (line), code);
}
