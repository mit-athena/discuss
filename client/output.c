/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.6 1988-12-02 23:37:54 raeburn Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board.
 *	
 *	Utility routines.
 *
 */

#ifndef lint
static char *rcsid_discuss_utils_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.6 1988-12-02 23:37:54 raeburn Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <strings.h>
#include <signal.h>
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"
#include "discuss_err.h"
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
	trn_info tinfo;

	*code = 0;
	dsc_get_trn_info(&dsc_public.nb, txn_no,
			 &tinfo, code);
	if (*code != 0) return;

	(void) strcpy (newtime, short_time (&tinfo.date_entered));
	newtime [24] = '\0';			/* get rid of \n */

	if (tinfo.num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	(void) sprintf (line, "[%04d] %s  %s  %s (%d line%s)\n",
			tinfo.current, tinfo.author,
			dsc_public.m_info.long_name,
			newtime, tinfo.num_lines, plural);
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
		(void) sprintf (line, "--[%04d]--\f\n", tinfo.current);
	else if (tinfo.pref == 0)
		(void) sprintf (line, "--[%04d]-- (nref = [%04d])\f\n",
				tinfo.current, tinfo.nref);
	else if (tinfo.nref == 0)
		(void) sprintf (line, "--[%04d]-- (pref = [%04d])\f\n",
				tinfo.current, tinfo.pref);
	else
	     (void) sprintf (line,
			     "--[%04d]-- (pref = [%04d], nref = [%04d])\f\n",
			     tinfo.current, tinfo.pref, tinfo.nref);
	twrite (tf, line, strlen (line), code);
}
