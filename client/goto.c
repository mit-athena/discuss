/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v 1.10 1988-08-16 23:15:11 srz Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	Code for "goto" request in discuss.
 *
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v 1.10 1988-08-16 23:15:11 srz Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include <ctype.h>
#include "discuss_err.h"
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"
#include "rpc.h"
#include "globals.h"
#include "acl.h"
#include "dsc_et.h"

#ifdef	lint
#define	DONT_USE(var)	var=var;
#else	lint
#define	DONT_USE(var)	;
#endif	lint

#define	FREE(ptr)	{ if (ptr) free(ptr); }
#define max(a, b) ((a) > (b) ? (a) : (b))

extern ss_request_table discuss_cmds;

/* EXTERNAL ROUTINES */

char	*malloc(), *getenv(), *gets(), *ctime(), *error_message();
tfile	unix_tfile();

goto_mtg(argc, argv)
	int argc;
	char **argv;
{
	if (argc != 2) {
		(void) fprintf(stderr, "Usage:  %s mtg_name\n", argv[0]);
		return;
	}

	switch_to_mtg(argv[1]);
}

switch_to_mtg(name)
	char *name;
{
	int code;
	name_blk nb;
	
	leave_mtg();

	dsc_get_mtg (user_id, name, &nb, &code);
	if (code != 0) {
	     ss_perror(sci_idx, DISC_MTG_NOT_FOUND, name);
	     return;
	}

	switch_to_mtg_nb (&nb);
	dsc_destroy_name_blk (&nb);
}

switch_to_mtg_nb(nbp)
	name_blk *nbp;
{
	int code, have_a, have_w;
	char msgbuf[80];
	trn_info t_info;

	/* Check to see if we're switching to same meeting. */
	if (dsc_public.attending 
	    && !strcmp(dsc_public.path, nbp->pathname)
	    && !strcmp(dsc_public.host, nbp ->hostname)) {
	    nbp -> date_attended = time((int *) 0);
	    nbp -> last = dsc_public.highest_seen;
        }

	leave_mtg();

	dsc_copy_name_blk(nbp, &dsc_public.nb);
	dsc_public.host = dsc_public.nb.hostname; /* warning - sharing */
	dsc_public.path = dsc_public.nb.pathname;
	dsc_get_mtg_info(&dsc_public.nb,
			 &dsc_public.m_info, &code);
	if (code != 0) {
	        if (code == NO_ACCESS)
		     code = CANT_ATTEND;
		(void) fprintf(stderr,
			       "Error going to %s: %s\n", 
			       dsc_public.nb.aliases[0], error_message(code));
		dsc_public.host = (char *)NULL;
		return;
	}

	dsc_public.mtg_name = (char *)malloc((unsigned)strlen(dsc_public.m_info.long_name)+1);
	(void) strcpy(dsc_public.mtg_name, dsc_public.m_info.long_name);

	dsc_public.attending = TRUE;
        dsc_public.highest_seen = dsc_public.current = dsc_public.nb.last;

	/* Check to see if meeting has been accidentally truncated */
	if (dsc_public.highest_seen > dsc_public.m_info.highest) {
	     printf ("Warning:  Transactions appear to have been lost.\n");
	     dsc_public.highest_seen = dsc_public.current = 0;
	}
	
	printf ("%s meeting:  %d new, %d last.",
		dsc_public.m_info.long_name,
		max (dsc_public.m_info.last - dsc_public.highest_seen, 0),
		dsc_public.m_info.last);
	if (acl_is_subset("c", dsc_public.m_info.access_modes)) 
		strcpy(msgbuf, "you are a chairman");
	else
		msgbuf[0] = '\0';
	have_w = acl_is_subset("w", dsc_public.m_info.access_modes);
	have_a = acl_is_subset("a", dsc_public.m_info.access_modes);
	if (have_w && have_a)
		goto done;
	if (msgbuf[0])
		strcat(msgbuf, "; ");
	if (!have_w)
		strcat(msgbuf,
		       have_a ? "reply access only" : "read access only");
	else
		strcat(msgbuf, "no replies");
 done:
	if (msgbuf[0]) {
		msgbuf[0] = toupper(msgbuf[0]);
		printf(" (%s.)", msgbuf);
	}
	printf("\n");

	/* See if our current transaction is deleted.  If so,
	   rewind ourselves to the previous non-deleted transaction.
	   Sigh.  This will be slow if there is a lot of deleted
	   transactions.  Can you say "Forced to be like forum?" */
	if (dsc_public.current != 0) {
	     if (dsc_public.current < dsc_public.m_info.first)
		  dsc_public.current = dsc_public.m_info.first;	/* don't fall off the ends */
	     else if (dsc_public.current > dsc_public.m_info.last)
		  dsc_public.current = dsc_public.m_info.last;
	     else {
		  code = DELETED_TRN;
		  while (code == DELETED_TRN) {
		       dsc_get_trn_info (&dsc_public.nb, dsc_public.current, &t_info, &code);
		       if (code == 0) {
			    free(t_info.subject); /* don't need these */
			    t_info.subject = NULL;
			    free(t_info.author);
			    t_info.author = NULL;
		       } else if (code == DELETED_TRN)
			    dsc_public.current--;
		       else {
			    ss_perror(sci_idx, code, "Looking for non-deleted transaction");
			    break;
		       }
		  }		  
	     }
	}
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
     if (dsc_public.host == (char *)NULL) {
	  fprintf (stderr, "leave: Inconsistent meeting state\n");
	  return;
     }

     dsc_public.nb.date_attended = time((int *)0);
     dsc_public.nb.last = dsc_public.highest_seen;
     dsc_update_mtg_set (user_id, &dsc_public.nb, 1, &code);

     dsc_destroy_name_blk(&dsc_public.nb);
     
     /* done with everything.  start nuking stuff */
     dsc_public.current = 0;
     dsc_public.highest_seen = 0;
     dsc_public.attending = FALSE;
     dsc_public.host = (char *)NULL;
     dsc_public.path = (char *)NULL;
     
     /* Don't forget the women and children... */
     FREE(dsc_public.mtg_name);
     dsc_public.mtg_name = (char *)NULL;

     dsc_destroy_mtg_info(&dsc_public.m_info);
}
