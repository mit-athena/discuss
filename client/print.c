/*
 *
 *	Print-related requests for DISCUSS.
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v 1.5 1986-10-15 00:50:23 spook Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *      $Log: not supported by cvs2svn $
 * Revision 1.4  86/10/15  00:18:26  spook
 * Added trn list parsing to print and write requests.
 * 
 * Revision 1.3  86/09/10  18:57:35  wesommer
 * Made to work with kerberos; meeting names are now longer.
 * ./
 * 
 * Revision 1.2  86/09/10  17:43:16  wesommer
 * Ken, please clean up after yourself.
 * 
 * Revision 1.1  86/08/22  00:24:01  spook
 * Initial revision
 * 
 * 
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v 1.5 1986-10-15 00:50:23 spook Exp $";
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
#include "../include/dsc_et.h"
#include "globals.h"

#ifdef	lint
#define	USE(var)	var=var;
#else	lint
#define	USE(var)	;
#endif	lint

extern tfile	unix_tfile();
static trn_nums	new_trn_no;
static char *	request_name;
static trn_info	t_info;
static tfile	tf;

static error_code
display_trans(trn_no)
	int trn_no;
{
	error_code code;
	output_trans(trn_no, tf, &code);
	if (code) {
		fprintf(stderr, "Error printing transaction: %s\n",
			error_message(code));
	}
	else if (new_trn_no == -1)
		new_trn_no = trn_no;
	return(code);
}

prt_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	int fd;
	int (*old_sig)();
	error_code code;
	selection_list *trn_list;

	request_name == ss_name(sci_idx);
	if (cur_mtg == (char *)NULL) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	get_mtg_info(cur_mtg, &m_info, &code);
	if (code != 0) {
		(void) ss_perror(sci_idx, code, "Can't get meeting info");
		return;
	}
	get_trn_info(cur_mtg, cur_trans, &t_info, &code);
	if (code)
		t_info.current = -1;

	if (argc == 1) {
		(void) fprintf(stderr, "Usage:  %s trn_no\n", argv[0]);
		return;
	}
	else if (argc == 2) {
		trn_list = trn_select(&t_info, argv[1],
				      (selection_list *)NULL, &code);
		if (code) {
			ss_perror(sci_idx, code, "");
			sl_free(trn_list);
			return;
		}
	}
	else {
		trn_list = (selection_list *)NULL;
		while (argv++, argc-- > 1) {
			trn_list = trn_select(&t_info, *argv,
					      trn_list, &code);
			if (code) {
				ss_perror(sci_idx, code, *argv);
				sl_free(trn_list);
				return;
			}
		}
	}

	new_trn_no = -1;
	/*
	 * Ignore SIGPIPE from the pager
	 */
	old_sig = signal(SIGPIPE, SIG_IGN);
	fd = ss_pager_create();
	if (fd < 0) {
		fprintf(stderr, "%s: Can't start pager: %s\n",
			request_name, error_message(ERRNO));
		return;
	}
	tf = unix_tfile(fd);
	(void) sl_map(display_trans, trn_list);
	tclose(tf, &code);
	(void) close(fd);
	(void) tdestroy(tf);
	(void) wait((union wait *)0);
	(void) signal(SIGPIPE, old_sig);
	if (new_trn_no == -1)
		(void) fprintf(stderr, "print: No transactions selected\n");
	else
		cur_trans = new_trn_no;
}

write_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	selection_list *trn_list;
	int fd;
	int (*old_sig)();
	int code;

	if (cur_mtg == (char *)NULL) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	get_mtg_info(cur_mtg, &m_info, &code);
	if (code != 0) {
		(void) ss_perror(sci_idx, code, "Can't get meeting info");
		return;
	}
	if (argc != 3) {
		(void) fprintf(stderr,
			       "Usage:  %s transaction_list filename\n",
			       argv[0]);
		return;
	}
	trn_list = trn_select(&t_info, argv[1], (selection_list *)NULL,
			      &code);
	if (code) {
		ss_perror(sci_idx, code, argv[1]);
		sl_free(trn_list);
		return;
	}
	new_trn_no = -1;
	cur_trans = txn_no;

	fd = open(argv[2], O_CREAT|O_APPEND|O_WRONLY, 0666);
	if (fd < 0) {
		ss_perror(sci_idx, errno, "Can't open output file");
		return;
	}
	tf = unix_tfile(fd);
	code = sl_map(display_trans, trn_list);
	if (code) {
		fprintf(stderr, "Error printing transaction: %s\n",
			error_message(code));
	}
	tclose(tf, &code);
	(void) close(fd);
	(void) tdestroy(tf);
}
