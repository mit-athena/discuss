/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.2 1987-01-04 19:30:10 srz Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board.
 *	
 *	Utility routines.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.1  86/12/07  01:31:16  rfrench
 * Initial revision
 * 
 * Revision 1.10  86/12/07  00:39:01  rfrench
 * Killed ../include
 * 
 * Revision 1.9  86/11/11  16:32:44  spook
 * Fixed to work with changes in et stuff
 * 
 * Revision 1.8  86/10/27  16:48:15  wesommer
 * Added form-feeds after each transaction.
 * 
 * Revision 1.7  86/10/27  16:29:04  wesommer
 * Damnit, folks, use RCS.  
 * 
 * Revision 1.6  86/10/19  09:58:52  spook
 * Changed to use dsc_ routines; eliminate refs to rpc.
 * 
 * Revision 1.5  86/09/13  20:31:56  srz
 * Include file fix
 * 
 * Revision 1.4  86/08/22  00:20:38  spook
 * new error-table stuff; separated routines
 * 
 * Revision 1.3  86/08/01  02:41:59  spook
 * Moved edit() from discuss.c; made edit() ignore SIGINT while waiting
 * for editor process to exit.
 * 
 *
 */

#ifndef lint
static char *rcsid_discuss_utils_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/output.c,v 1.2 1987-01-04 19:30:10 srz Exp $";
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
extern char *malloc(), *getenv(), *ctime();

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
	dsc_get_trn_info(dsc_public.mtg_uid, txn_no, &tinfo, code);
	if (*code != 0) return;

	(void) strcpy (newtime, ctime (&(tinfo.date_entered)));
	newtime [24] = '\0';			/* get rid of \n */

	if (tinfo.num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	(void) sprintf (line, "[%04d] %s  %s  %s (%d line%s)\n",
			tinfo.current, tinfo.author,
			dsc_public.m_info.long_name,
			&newtime[4], tinfo.num_lines, plural);
	twrite (tf, line, strlen (line), code);
	if (tinfo.subject [0] != '\0') {
		twrite (tf, "Subject: ", 9, code);
		twrite (tf, tinfo.subject, strlen (tinfo.subject), code);
		twrite (tf, "\n", 1, code);
	}
	dsc_get_trn(dsc_public.mtg_uid, txn_no, tf, code);
	if (*code != 0) return;

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
