/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/randrp.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/randrp.c,v 1.2 1988-01-04 22:40:46 balamac Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1988 by the Student Information Processing Board
 *
 *	Code for "randrp" request in discuss.
 *
 */

#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/randrp.c,v 1.2 1988-01-04 22:40:46 balamac Exp $";
#endif lint

#include "types.h"
#include "interface.h"
#include "globals.h"
#include <stdio.h>

randrp(argc, argv, sci_idx)
	int argc;
	char **argv;
	int sci_idx;
{
	char *meeting = NULL;
	char *editor = NULL;
	char *trans = NULL;
	int code;
	int active_transactions;
	long rnd_num;
	int rnd_trn;
	int noeditor = FALSE;
	trn_info t_info;

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

	do {
		rnd_num = random();
		active_transactions =
			(dsc_public.m_info.last - dsc_public.m_info.first);
		rnd_trn = (dsc_public.m_info.first +
			   (rnd_num % active_transactions));
		dsc_get_trn_info(&dsc_public.nb, rnd_trn, &t_info, &code);
	} while (code != 0);
		
	if ((editor != NULL) && !noeditor) {
		(void) sprintf(buffer, "reply -editor %s %d", editor, rnd_trn);
		ss_execute_line(sci_idx, buffer, &code);
	}
	else if (noeditor) {
		(void) sprintf(buffer, "reply -no_editor %d",rnd_trn);
		ss_execute_line(sci_idx, buffer, &code);
	}
	else {
		(void) sprintf(buffer, "reply %d",rnd_trn);
		ss_execute_line(sci_idx, buffer, &code);
	}

	if (code != 0) {
		ss_perror(sci_idx, code, buffer);
		return;
	}

	return;
}

