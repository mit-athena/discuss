/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.9 1986-08-23 21:42:48 spook Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	A simple shell-type user interface to discuss; uses Ken Raeburn's
 *	ss library for the command interpreter.
 *
 *      $Log: not supported by cvs2svn $
 * Revision 1.8  86/08/22  00:19:19  spook
 * using new error-table stuff; moved some code out to other
 * modules
 * 
 * Revision 1.7  86/08/07  13:40:44  spook
 * replaced "/projects/discuss/client/info" with #define from config.h
 * 
 * Revision 1.6  86/08/02  14:01:11  wesommer
 * Fixed to ignore SIGPIPE if the pager goes away.
 * 
 * Revision 1.5  86/08/01  02:41:35  spook
 * Moved edit() to discuss_utils.c.
 * 
 * Revision 1.4  86/07/31  15:56:08  wesommer
 * Fixed up some brain-damage surrounding the prt_trans/write_trans
 * interactions.
 *      "If you're using longjmp, you're doing something wrong"
 *                              - Jim Gettys
 * write_trans no longer takes an sci_idx as its argument; it has an
 * additional last argument, which is an error code.
 * 
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.9 1986-08-23 21:42:48 spook Exp $";
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

extern ss_request_table discuss_cmds;
trn_nums cur_trans = -1;
char	*cur_mtg = (char *)NULL;
char	*temp_file = (char *)NULL;
char	*pgm = (char *)NULL;
char	*malloc(), *getenv(), *gets(), *ctime();
mtg_info m_info;
char	buf[BUFSIZ];
char	*buffer = &buf[0];
tfile	unix_tfile();
int	dsc_sci_idx;

main(argc, argv)
	int argc;
	char **argv;
{
	int sci_idx;
	int code;

	sci_idx = ss_create_invocation("discuss", CURRENT_VERSION,
				       (char *)NULL, &discuss_cmds, &code);
	if (code) {
		ss_perror(sci_idx, code, "creating invocation");
		exit(1);
	}
	dsc_sci_idx = sci_idx;
	ss_add_info_dir(sci_idx, INFO_DIR, &code);
	if (code) {
		ss_perror(sci_idx, code, INFO_DIR);
		exit(1);
	}

	init_disc_err_tbl();
	init_rpc();

	temp_file = malloc(64);
	pgm = malloc(64);
	(void) sprintf(temp_file, "/tmp/mtg%d.%d", getuid(), getpid());

	if (argc != 1) {
		(void) sprintf(buffer, "goto %s", argv[1]);
		ss_execute_line(sci_idx, buffer, &code);
		if (code != 0)
			ss_perror(sci_idx, code, argv[1]);
	}
	ss_listen (sci_idx, &code);
	(void) unlink(temp_file);
}

repl(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int fd, txn_no;
	tfile tf;
	trn_info t_info;
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
	if (cur_trans == -1) {
		(void) fprintf(stderr, "No current transaction.\n");
		return;
	}
	get_trn_info(cur_mtg, cur_trans, &t_info, &code);
	if (code != 0) {
		(void) fprintf(stderr,
			       "Can't get info on current transaction.  Error %d.\n",
			       code);
		return;
	}
	if (strncmp(t_info.subject, "Re: ", 4)) {
		char *new_subject = malloc((unsigned)strlen(t_info.subject)+5);
		(void) strcpy(new_subject, "Re: ");
		(void) strcat(new_subject, t_info.subject);
		(void) free(t_info.subject);
		t_info.subject = new_subject;
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
	
	add_trn(cur_mtg, tf, t_info.subject,
		cur_trans, &txn_no, &code);
	if (code != 0) {
		(void) fprintf(stderr, "Error %d.\n", code);
		return;
	}
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		      txn_no, cur_mtg);
	cur_trans = txn_no;
}

del_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	int code;
	USE(sci_idx);
	if (cur_mtg == (char *)NULL) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	if (argc != 2) {
		(void) fprintf(stderr, "Usage:  %s trn_no\n", argv[0]);
		return;
	}
	txn_no = atoi(argv[1]);
	delete_trn(cur_mtg, txn_no, &code);
	if (code != 0) {
		(void) fprintf(stderr, "Error %d.\n", code);
		return;
	}
	cur_trans = txn_no + 1;
}

ret_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	int code;
	USE(sci_idx);
	if (cur_mtg == (char *)NULL) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	if (argc != 2) {
		(void) fprintf(stderr, "Usage:  %s trn_no\n", argv[0]);
		return;
	}
	txn_no = atoi(argv[1]);
	retrieve_trn(cur_mtg, txn_no, &code);
	if (code != 0) {
		(void) fprintf(stderr, "Error %d.\n", code);
		return;
	}
	cur_trans = txn_no;
}

goto_mtg(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	char *path;
	int code;
	USE(sci_idx);
	if (argc != 2) {
		(void) fprintf(stderr, "Usage:  %s mtg_name\n", argv[0]);
		return;
	}
	if (cur_mtg != (char *)NULL)
		(void) free(cur_mtg);
	cur_mtg = (char *)NULL;
	path = malloc((unsigned)((strlen(argv[1]) + 20) * sizeof(char)));
	(void) strcpy(path, "/usr/spool/discuss/");
	(void) strcat(path, argv[1]);
	get_mtg_info(path, &m_info, &code);
	if (code != 0) {
		if (code == NO_SUCH_MTG)
			(void) fprintf(stderr, "No such meeting. %s\n",
				       argv[1]);
		else if (code == BAD_MTG_NAME)
			(void) fprintf(stderr, "Bad meeting name. %s\n",
				       argv[1]);
		else
			(void) fprintf(stderr, "Error %d.\n", code);
		return;
	}
	cur_mtg = path;
}
