/*
 *
 *	Print-related requests for DISCUSS.
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v 1.2 1986-09-10 17:43:16 wesommer Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *      $Log: not supported by cvs2svn $
 * Revision 1.1  86/08/22  00:24:01  spook
 * Initial revision
 * 
 * 
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v 1.2 1986-09-10 17:43:16 wesommer Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include "../include/ss.h"
#include "../include/tfile.h"
#include "../include/interface.h"
#include "../include/config.h"
#include "globals.h"

#ifdef	lint
#define	USE(var)	var=var;
#else	lint
#define	USE(var)	;
#endif	lint

extern tfile	unix_tfile();

prt_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	tfile tf;
	int fd;
	int (*old_sig)();
	int code;

	if (cur_mtg == (char *)NULL) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	if (argc != 2) {
		(void) fprintf(stderr, "Usage:  %s trn_no\n", argv[0]);
		return;
	}
	txn_no = atoi(argv[1]);
	cur_trans = txn_no;
	/*
	 * Ignore SIGPIPE from the pager
	 */
	old_sig = signal(SIGPIPE, SIG_IGN);
	fd = pager_create();
	if (fd < 0) {
		ss_perror(sci_idx, errno, "Can't start pager");
		return;
	}
	tf = unix_tfile(fd);
	output_trans(txn_no, tf, &code);
	if(code) {
		if (code == DELETED_TRN)
			(void) fprintf(stderr,
				       "Transaction has been deleted.\n");
		else if (code == NO_SUCH_TRN)
			(void) fprintf(stderr, "No such transaction.\n");
		else
			(void) fprintf(stderr, "Error %d.\n", code);
	}
	tclose(tf, &code);
	(void) close(fd);
	(void) tdestroy(tf);
	(void) wait((union wait *)0);
	(void) signal(SIGPIPE, old_sig);
}

write_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	tfile tf;
	int fd;
	int (*old_sig)();
	int code;

	if (cur_mtg == (char *)NULL) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	if (argc != 3) {
		(void) fprintf(stderr, "Usage:  %s trn_no filename\n",
			       argv[0]);
		return;
	}
	txn_no = atoi(argv[1]);
	cur_trans = txn_no;

	fd = open(argv[argc-1], O_CREAT|O_APPEND|O_WRONLY, 0666);
	if (fd < 0) {
		ss_perror(sci_idx, errno, "Can't open output file");
		return;
	}
	tf = unix_tfile(fd);
	output_trans(txn_no, tf, &code);
	if (code) {
		if (code == DELETED_TRN)
			(void) fprintf(stderr,
				       "Transaction has been deleted.\n");
		else if (code == NO_SUCH_TRN)
			(void) fprintf(stderr, "No such transaction.\n");
		else
			(void) fprintf(stderr, "Error %d.\n", code);
	}
	tclose(tf, &code);
	(void) close(fd);
	(void) tdestroy(tf);
}
