/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/randrp.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/randrp.c,v 1.10 1994-03-25 16:25:07 miki Exp $
 *	$Locker:  $
 *
 *	Code for "randrp" request in discuss.
 *
 */

#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/randrp.c,v 1.10 1994-03-25 16:25:07 miki Exp $";
#endif lint

#include <discuss/discuss.h>
#include "globals.h"
#include <stdio.h>
#include <sys/time.h>
#ifdef SOLARIS
#define random lrand48
#define srandom srand48
#endif


randrp(argc, argv, sci_idx)
	int argc;
	char **argv;
	int sci_idx;
{
	char *meeting = NULL;
	char *editor = NULL;
	char *trans = NULL;
	void srandom(), gettimeofday();
	int getpid();
	struct timeval tv;
	int randrp_retry = 15;
	int i, code;
	int active_transactions;
	long rnd_num;
	int rnd_trn;
	int pid = getpid();
	int noeditor = FALSE;
	int prompt_subject = FALSE;
	trn_info t_info;
	trn_nums current_transaction;

	while (++argv, --argc) {
		if (!strcmp (*argv, "-meeting") || !strcmp (*argv, "-mtg")) {
			if (argc==1) {
				(void) fprintf(stderr, 
					       "No argument to %s.\n", *argv);
				return;
			} else {
				--argc;
				meeting = *(++argv);
			}
		} else if (!strcmp (*argv, "-editor") || !strcmp(*argv, "-ed")) {
			if (argc==1) {
				(void) fprintf(stderr, 
					       "No argument to %s.\n", *argv);
				return;
			} else {
				--argc;
				editor = *(++argv);
			}
		} else if (!strcmp(*argv, "-no_editor")) {
			noeditor = TRUE;
		} else if (!strcmp(*argv, "-subject") ||
			   !strcmp(*argv, "-sj")) {
		    /* prompt for subject */   
		    prompt_subject = TRUE;
		} else {
			ss_perror(sci_idx, 0,
				  "Cannot specify transaction in random reply");
		return; }
	}

	if (meeting) {
		(void) sprintf(buffer, "goto %s", meeting);
		ss_execute_line(sci_idx, buffer, &code);
		if (code != 0) {
			ss_perror(sci_idx, code, buffer);
			return;
		}
	}

	if (!dsc_public.attending) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		return;
	}

	/* Need to preserve current transaction across the call to reply */

	current_transaction = dsc_public.current;

	gettimeofday(&tv, (struct timezone *) NULL);
	srandom(tv.tv_sec ^ tv.tv_usec ^ pid);

	/* improved randomization routine...
	   The idea here is to look for the first transaction in a chain.
	   Try randrp_retry times.  If we find one before then, fine.
	   If not, use the last txn found.  This increases the distribution
	   of randrp subject lines.

	   BAL 9/24/88  */

	for (i=1;i<=randrp_retry;i++) {
		do {
			rnd_num = random();
			active_transactions =
				(dsc_public.m_info.last - dsc_public.m_info.first);
			if (active_transactions != 0) {
				rnd_trn = (dsc_public.m_info.first +
					   (rnd_num % active_transactions));
			} else {
				rnd_trn = dsc_public.m_info.first;
			}
			dsc_get_trn_info(&dsc_public.nb, rnd_trn, &t_info, &code);
			dsc_destroy_trn_info(&t_info);
		} while (code != 0);
		if (!t_info.pref) break;
	}
		
	if ((editor != NULL) && !noeditor) {
		(void) sprintf(buffer, "reply %s-editor %s %d",
			       prompt_subject ? "-subject " : "",
			       editor, rnd_trn);
		ss_execute_line(sci_idx, buffer, &code);
	}
	else if (noeditor) {
		(void) sprintf(buffer, "reply %s-no_editor %d",
			       prompt_subject ? "-subject " : "",rnd_trn);
		ss_execute_line(sci_idx, buffer, &code);
	}
	else {
		(void) sprintf(buffer, "reply %s%d",
			       prompt_subject ? "-subject " : "",rnd_trn);
		ss_execute_line(sci_idx, buffer, &code);
	}

	dsc_public.current = current_transaction;
	if (code != 0) {
		ss_perror(sci_idx, code, buffer);
		return;
	}

	return;
}

