/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss_utils.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss_utils.c,v 1.3 1986-08-01 02:41:59 spook Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board.
 *	
 *	Utility routines.
 *
 *	$Log: not supported by cvs2svn $
 *
 */

#ifndef lint
static char *rcsid_discuss_utils_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss_utils.c,v 1.3 1986-08-01 02:41:59 spook Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <strings.h>
#include <signal.h>
#include "../include/ss.h"
#include "../include/tfile.h"
#include "../include/interface.h"
#include "../include/config.h"

extern ss_request_table discuss_cmds;
extern int current_trans;
extern char *cur_mtg;
extern int l_zcode;
extern char *temp_file;
extern char *pgm;
extern char *malloc(), *getenv();
extern mtg_info m_info;
extern char buffer[BUFSIZ];

write_trans(txn_no, tf, code)
	trn_nums txn_no;
	tfile tf;
	int *code;
{
	char *plural;
	char newtime[26];
	char line[255];
	trn_info tinfo;

	*code = 0;
	get_trn_info(cur_mtg, txn_no, &tinfo, code);
	if (*code != 0) return;

	strcpy (newtime, ctime (&(tinfo.date_entered)));
	newtime [24] = '\0';			/* get rid of \n */

	if (tinfo.num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	sprintf (line, "[%04d] %s %s %s (%d line%s)\n",
		 tinfo.current, tinfo.author, m_info.long_name, &newtime[4],
		 tinfo.num_lines, plural);
	twrite (tf, line, strlen (line));
	if (tinfo.subject [0] != '\0') {
		twrite (tf, "Subject: ", 9);
		twrite (tf, tinfo.subject, strlen (tinfo.subject));
		twrite (tf, "\n", 1);
	}
	get_trn(cur_mtg, txn_no, tf, code);
	if (*code != 0) return;

	if (tinfo.pref == 0 && tinfo.nref == 0)
		sprintf (line, "--[%04d]--\n\n", tinfo.current);
	else if (tinfo.pref == 0)
		sprintf (line, "--[%04d]-- (nref = [%04d])\n\n", tinfo.current,
			 tinfo.nref);
	else if (tinfo.nref == 0)
		sprintf (line, "--[%04d]-- (pref = [%04d])\n\n", tinfo.current,
			 tinfo.pref);
	else
		sprintf (line, "--[%04d]-- (pref = [%04d], nref = [%04d])\n\n",
			 tinfo.current, tinfo.pref, tinfo.nref);
	twrite (tf, line, strlen (line));
}

edit(fn)
	char *fn;
{
	int pid;
	char *ed = getenv("EDITOR");
	int (*handler)();

	if (!ed) ed="/bin/ed";

	switch ((pid = fork())) {
	case -1:
		perror("couldn't fork");
		break;
	case 0:
		execlp(ed, ed, fn, 0);
		perror(ed);
		exit(1);
		break;
	default:
		break;
	}
	handler = signal(SIGINT, SIG_IGN);
	while (wait(0) != pid)
		;
	signal(SIGINT, handler);
}
