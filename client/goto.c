/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v 1.1 1987-03-22 04:33:49 spook Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	Code for "goto" request in discuss.
 *
 *      $Log: not supported by cvs2svn $
 * 
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v 1.1 1987-03-22 04:33:49 spook Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"
#include "rpc.h"
#include "globals.h"
#include "acl.h"

#ifdef	lint
#define	DONT_USE(var)	var=var;
#else	lint
#define	DONT_USE(var)	;
#endif	lint

#define	FREE(ptr)	{ if (ptr) free(ptr); }
#define max(a, b) ((a) > (b) ? (a) : (b))

extern ss_request_table discuss_cmds;

/* EXTERNAL ROUTINES */

char	*malloc(), *getenv(), *gets(), *ctime();
tfile	unix_tfile();

#define DEFAULT_EDITOR "/bin/ed"

goto_mtg(argc, argv)
	int argc;
	char **argv;
{
	int code;

	DONT_USE(sci_idx);
	if (argc != 2) {
		(void) fprintf(stderr, "Usage:  %s mtg_name\n", argv[0]);
		return;
	}

	leave_mtg();

	dsc_public.mtg_name = (char *)malloc((unsigned)strlen(argv[1])+1);
	(void) strcpy(dsc_public.mtg_name, argv[1]);

	get_mtg_unique_id (user_id, dsc_public.mtg_name, &dsc_public.nb,
			   &code);
	if (code != 0) {
		(void) fprintf (stderr,
				"%s: Meeting not found in search path.\n",
				argv[1]);
		return;
	}

	dsc_public.mtg_uid = dsc_public.nb.unique_id;	/* warning - sharing */
	dsc_get_mtg_info(dsc_public.mtg_uid, &dsc_public.m_info, &code);
	if (code != 0) {
		(void) fprintf(stderr,
			       "Error getting meeting info for %s: %s\n", 
			       dsc_public.mtg_name, error_message(code));
		dsc_public.mtg_uid = (char *)NULL;
		return;
	}
	dsc_public.attending = TRUE;
        dsc_public.highest_seen = dsc_public.current = dsc_public.nb.last;
	printf ("%s meeting;  %d new, %d last",
		dsc_public.m_info.long_name,
		max (dsc_public.m_info.last - dsc_public.highest_seen, 0),
		dsc_public.m_info.last);
	if (acl_is_subset("c", dsc_public.m_info.access_modes)) 
		printf(" (You are a chairman)");
	if (!acl_is_subset("w", dsc_public.m_info.access_modes)) {
		if (!acl_is_subset("a", dsc_public.m_info.access_modes)) 
			printf(" (Read only)");
		else printf(" (Reply only)");
	} else if (!acl_is_subset("a", dsc_public.m_info.access_modes))
		printf(" (No replies)");
	printf(".\n\n");
}

/*
 *
 * leave_mtg () -- Internal routine to leave the current meeting, updating
 *		   all the stuff we need.  Not a light-weight operation.
 *
 */

leave_mtg()
{
     int code;

     if (!dsc_public.attending)
	  return;				/* bye, jack */
     if (dsc_public.mtg_uid == (char *)NULL) {
	  fprintf (stderr, "leave: Inconsistent meeting state\n");
	  return;
     }

     dsc_public.nb.date_attended = time((int *)0);
     dsc_public.nb.last = dsc_public.highest_seen;
     update_mtg_set (user_id, &dsc_public.nb, 1, &code);

     /* done with everything.  start nuking stuff */
     dsc_public.current = 0;
     dsc_public.highest_seen = 0;
     dsc_public.attending = FALSE;
     dsc_public.mtg_uid = (char *)NULL;

     /* Don't forget the women and children... */
     FREE(dsc_public.mtg_name);
     dsc_public.mtg_name = (char *)NULL;
     FREE(dsc_public.m_info.chairman);
     dsc_public.m_info.chairman = (char *)NULL;
     FREE(dsc_public.m_info.location);
     dsc_public.m_info.location = (char *)NULL;
}
