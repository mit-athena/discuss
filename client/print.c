/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	Print-related requests for DISCUSS.
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v 1.23 1996-04-05 22:24:47 ghudson Exp $
 *	$Locker:  $
 *
 */


#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/print.c,v 1.23 1996-04-05 22:24:47 ghudson Exp $";
#endif /* lint */

#include <stdio.h>
#include <errno.h>
#include <sys/file.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#ifdef SOLARIS
#include <fcntl.h>
#endif
#include <discuss/discuss.h>
#include "ss.h"
#include "config.h"
#include "globals.h"

#ifdef	lint
#define	USE(var)	var=var;
#else	lint
#define	USE(var)	;
#endif	lint

#define max(a, b) ((a) > (b) ? (a) : (b))

extern tfile	unix_tfile();
static trn_nums	performed;
static char *	request_name;
static trn_info	t_info;
static tfile	tf;

extern void sl_free();
extern char *error_message();

static int
display_trans(t_infop, codep)
trn_info3 *t_infop;
int *codep;
{
	output_trans(t_infop, tf, codep);
	if (*codep == 0) {
	     dsc_public.highest_seen = max(dsc_public.highest_seen,t_infop->current);
	     dsc_public.current = t_infop->current;
	     performed = TRUE;
	} else if (*codep == EPIPE) {
	     dsc_public.current = t_infop->current;
	     performed = TRUE;
	     return;			/* silently quit */
	}
	else if (*codep != DELETED_TRN) {
	     fprintf(stderr, "Error printing transaction: %s\n",
		     error_message(*codep));
	     return;
	}

	*codep = 0;
	return;
}

prt_trans(argc, argv)
	int argc;
	char **argv;
{
	int fd;
#ifndef POSIX
	int (*old_sig)();
#endif
	int code;
	selection_list *trn_list;
#ifdef POSIX
      struct sigaction act, oact;
#endif

	request_name = ss_name(sci_idx);

	if (argc != 1) {
		if (strcmp(argv[0], "print") &&
		    strcmp(argv[0], "pr") &&
		    strcmp(argv[0], "p")) {
		     	fprintf(stderr, "Usage: %s\n", argv[0]);
			return;
		}
	}

	if (!dsc_public.attending) {
	     ss_perror(sci_idx, DISC_NO_MTG, "");
	     return;
	}
	dsc_destroy_mtg_info(&dsc_public.m_info);
	dsc_get_mtg_info(&dsc_public.nb,
			 &dsc_public.m_info, &code);
	if (code != 0) {
	     (void) ss_perror(sci_idx, code, "Can't get meeting info");
	     return;
	}

	dsc_get_trn_info(&dsc_public.nb, dsc_public.current,
			 &t_info, &code);
	if (code == DELETED_TRN) {
	        t_info.current = dsc_public.current;
		t_info.next = dsc_public.current+1;
		t_info.prev = dsc_public.current-1;
	} else if (code)
		t_info.current = 0;

	dsc_destroy_trn_info(&t_info);

	if (argc == 1) {
		char *ref;
		if (!strcmp(argv[0], "print") ||
		    !strcmp(argv[0], "pr") ||
		    !strcmp(argv[0], "p")) {
		        ref = "current";
		} else ref = argv[0];

	        trn_list = trn_select(&t_info, ref,
				      (selection_list *)NULL, &code);
		if (code) {
			ss_perror(sci_idx, code, "");
			sl_free(trn_list);
			return;
		}
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

	performed = FALSE;
	/*
	 * Ignore SIGPIPE from the pager
	 */
#ifdef POSIX
      sigemptyset(&act.sa_mask);
      act.sa_flags = 0;
      act.sa_handler= (void (*)()) SIG_IGN;
      (void) sigaction(SIGPIPE, &act, &oact);
#else
	old_sig = signal(SIGPIPE, SIG_IGN);
#endif
	fd = ss_pager_create();
	if (fd < 0) {
	     ss_perror(sci_idx, errno, "Can't start pager");
	     return;
	}
	tf = unix_tfile(fd);
	(void) sl_map(display_trans, trn_list,FALSE);
	sl_free(trn_list);
	tclose(tf, &code);
	(void) close(fd);
	(void) tdestroy(tf);
#ifdef SOLARIS
	(void) wait((int  *)0);
#else
	(void) wait((union wait *)0);
#endif
#ifdef POSIX
      (void) sigaction (SIGPIPE, &oact, NULL);
#else
	(void) signal(SIGPIPE, old_sig);
#endif
	if (!performed)
	     ss_perror(sci_idx, DISC_NO_TRN, "");
}

write_trans(argc, argv)
	int argc;
	char **argv;
{
	selection_list *trn_list;
	int fd;
	int code;
	char *arg, *filename;

	if (dsc_public.host == (char *)NULL) {
	     ss_perror(sci_idx, DISC_NO_MTG, "");
	     return;
	}
	dsc_destroy_mtg_info(&dsc_public.m_info);
	dsc_get_mtg_info(&dsc_public.nb,
			 &dsc_public.m_info, &code);
	if (code != 0) {
		(void) ss_perror(sci_idx, code, "Can't get meeting info");
		return;
	}
	if (argc == 3) {
	     arg = argv[1];
	     filename = argv[2];
	}
	else if (argc == 2) {
	     arg = "current";
	     filename = argv[1];
	}
	else {
	     (void) fprintf(stderr,
			    "Usage:  %s transaction_list filename\n",
			    argv[0]);
	     return;
	}
	trn_list = trn_select(&t_info, arg, (selection_list *)NULL, &code);
	if (code) {
		ss_perror(sci_idx, code, arg);
		sl_free(trn_list);
		return;
	}
	performed = FALSE;

	fd = open(filename, O_CREAT|O_APPEND|O_WRONLY, 0666);
	if (fd < 0) {
		ss_perror(sci_idx, errno, "Can't open output file");
		return;
	}
	tf = unix_tfile(fd);
	(void) sl_map(display_trans, trn_list, FALSE);
	sl_free(trn_list);
	tclose(tf, &code);
	(void) close(fd);
	(void) tdestroy(tf);
	if (!performed)
	     ss_perror(sci_idx, DISC_NO_TRN, "");
	return;
}
