/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v 1.16 1992-12-23 11:47:30 probe Exp $
 *	$Locker:  $
 *
 *	Code for "goto" request in discuss.
 *
 */

#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/goto.c,v 1.16 1992-12-23 11:47:30 probe Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include <ctype.h>
#include "ss.h"
#include <discuss/discuss.h>
#include "globals.h"

#ifdef	lint
#define	DONT_USE(var)	var=var;
#else	lint
#define	DONT_USE(var)	;
#endif	lint

#define	FREE(ptr)	{ if (ptr) free(ptr); }
#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

extern ss_request_table discuss_cmds;

/* EXTERNAL ROUTINES */

char	*malloc(), *getenv(), *gets(), *ctime(), *error_message();
tfile	unix_tfile();
void	leave_mtg ();

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
    int code, have_a, have_w, dummy;
    char msgbuf[80],*old_hostname,*old_pathname;
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
	 while (code == MTG_MOVED) {
	      /* Meeting has moved.  In this case, dsc_public.m_info.long_name
		 should contain the hostname/pathname for the meeting.
		 We should update our information to reflect this change. */
	      old_hostname = dsc_public.nb.hostname;
	      dsc_public.nb.hostname = malloc(strlen(dsc_public.m_info.long_name)+1);
	      strcpy(dsc_public.nb.hostname, dsc_public.m_info.long_name);
	      old_pathname = dsc_public.nb.pathname;
	      dsc_public.nb.pathname = malloc(strlen(dsc_public.m_info.location)+1);
	      strcpy(dsc_public.nb.pathname, dsc_public.m_info.location);
	      dsc_public.host = dsc_public.nb.hostname; /* warning - sharing */
	      dsc_public.path = dsc_public.nb.pathname;
	      dsc_get_mtg_info(&dsc_public.nb,
			       &dsc_public.m_info, &code);
	      if (code != 0 && code != MTG_MOVED) {
		   fprintf(stderr, "Error checking moved meeting %s.  %s\n",
			   dsc_public.nb.aliases[0], error_message(code));
		   dsc_public.host = NULL;
	      } else {
		   fprintf(stdout, "Warning: %s moved to %s:%s\n",
			   dsc_public.nb.aliases[0], dsc_public.host, dsc_public.path);
		   /* Delete old meeting */
		   dsc_public.nb.hostname = old_hostname;
		   dsc_public.nb.pathname = old_pathname;
		   dsc_public.nb.status |= DSC_ST_DELETED;
		   dsc_update_mtg_set(user_id, &dsc_public.nb, 1, &dummy);
		   dsc_public.nb.status &= ~(DSC_ST_DELETED);
		   free(dsc_public.nb.hostname);
		   dsc_public.nb.hostname = dsc_public.host;
		   free(dsc_public.nb.pathname);
		   dsc_public.nb.pathname = dsc_public.path;
	      }
	 }
	 if (code != 0) {
	      if (code == NO_ACCESS)
		   code = CANT_ATTEND;
	      (void) fprintf(stderr,
			     "Error going to %s: %s\n", 
			     dsc_public.nb.aliases[0], error_message(code));
	      dsc_public.host = (char *)NULL;
	      return;
	 }
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
	/* the first character is currently lower-case */
	msgbuf[0] = toupper (msgbuf[0]);
	printf(" (%s.)", msgbuf);
    }
    printf("\n");
    
    /* See if our current transaction is deleted.  If so,
     * rewind ourselves to the previous non-deleted transaction.
     * Sigh.  This will be slow if there is a lot of deleted
     * transactions.  Can you say "Forced to be like forum?" */
    if (dsc_public.current == 0)
	return;
    if (dsc_public.current < dsc_public.m_info.first) {
	/* don't fall off the ends */
	dsc_public.current = dsc_public.m_info.first;
    }
    else if (dsc_public.current > dsc_public.m_info.last)
	dsc_public.current = dsc_public.m_info.last;
    else {
	code = DELETED_TRN;
	while (code == DELETED_TRN) {
	    dsc_get_trn_info (&dsc_public.nb, dsc_public.current,
			      &t_info, &code);
	    dsc_destroy_trn_info(&t_info);
	    if (code == DELETED_TRN)
		dsc_public.current--;
	    else if (code != 0) {
		ss_perror(sci_idx, code,
			  "Looking for non-deleted transaction");
		break;
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

void leave_mtg()
{
    int code;
    mtg_info minfo;

    if (!dsc_public.attending)
	return;			/* bye, jack */
    if (dsc_public.host == (char *)NULL) {
	ss_perror (sci_idx, 0, "program error: meeting state inconsistent!");
#ifdef DEBUG
	fprintf (stderr, "leave_mtg: attending meeting, no host\n");
	/* we should have some sort of debug mode... */
	abort ();
#endif
	return;
    }
    
    dsc_public.nb.date_attended = time((int *)0);
    dsc_public.nb.last = dsc_public.highest_seen;
    dsc_get_mtg_info (&dsc_public.nb, &minfo, &code);
    if (!code) {
	if (dsc_public.highest_seen == dsc_public.m_info.last)
	    dsc_public.nb.status &= ~ DSC_ST_CHANGED;
    }
    dsc_destroy_mtg_info (&minfo);
    dsc_update_mtg_set (user_id, &dsc_public.nb, 1, &code);
    
    dsc_destroy_name_blk (&dsc_public.nb);
    
    /* done with everything.  start nuking stuff */
    dsc_public.current = 0;
    dsc_public.highest_seen = 0;
    dsc_public.attending = FALSE;
    dsc_public.host = (char *)NULL;
    dsc_public.path = (char *)NULL;
    
    /* Don't forget the women and children... */
    FREE (dsc_public.mtg_name);
    dsc_public.mtg_name = (char *)NULL;
    
    dsc_destroy_mtg_info (&dsc_public.m_info);
}
