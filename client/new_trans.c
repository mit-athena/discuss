/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/new_trans.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/new_trans.c,v 1.2 1986-09-10 18:57:32 wesommer Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	New-transaction routine for DISCUSS.  (Request 'talk'.)
 *
 *      $Log: not supported by cvs2svn $
 * Revision 1.1  86/08/22  00:23:58  spook
 * Initial revision
 * 
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/new_trans.c,v 1.2 1986-09-10 18:57:32 wesommer Exp $";
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

new_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int fd, txn_no;
	tfile tf;
	char *subject = &buffer[0];
	int code;

	USE(sci_idx);
	if (cur_mtg == (char *)NULL) {
		(void) fprintf(stderr, "Not currently attending a meeting.\n");
		return;
	}
	if (argc != 1) {
		(void) fprintf(stderr, "Usage:  %s\n", argv[0]);
		return;
	}
	(void) printf("Subject: ");
	if (gets(subject) == (char *)NULL) {
		(void) fprintf(stderr, "Error reading subject.\n");
		return;
	}
	(void) unlink(temp_file);
	if (edit(temp_file) != 0) {
		(void) fprintf(stderr,
			       "Error during edit; transaction not entered\n");
		unlink(temp_file);
		return;
	}
	fd = open(temp_file, O_RDONLY, 0);
	if (fd < 0) {
		(void) fprintf(stderr, "No file; not entered.\n");
		return;
	}
	tf = unix_tfile(fd);
	add_trn(cur_mtg, tf, subject, 0, &txn_no, &code);
	if (code != 0) {
		(void) fprintf(stderr, "Error adding transaction: %s\n", 
			       error_message(code));
		return;
	}
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		      txn_no, cur_mtg);
	cur_trans = txn_no;
}

