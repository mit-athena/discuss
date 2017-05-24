/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: new_trans.c,v 1.30 2006-03-10 07:11:31 ghudson Exp $
 *
 *	New-transaction routine for DISCUSS.  (Request 'talk'.)
 *
 */


#ifndef lint
static char rcsid_discuss_c[] =
     "$Id: new_trans.c,v 1.30 2006-03-10 07:11:31 ghudson Exp $";
#endif /* lint */

#include <stdlib.h>
#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <ss/ss.h>
#include <discuss/discuss.h>
#include "config.h"
#include "globals.h"

#ifdef	lint
#define	USE(var)	var=var;
#else	/* lint */
#define	USE(var)	;
#endif	/* lint */

extern tfile	unix_tfile();
extern void flag_interrupts(), dont_flag_interrupts();

new_trans(argc, argv)
     int argc;
     char **argv;
{
     int fd, txn_no;
     tfile tf;
     char *subject = &buffer[0];
     char *whoami = argv[0];
     char *mtg = NULL;
     char *myname = NULL;
     int code;
     char *editor = NULL;

     USE(sci_idx);

     while (++argv, --argc) {
	  if (!strcmp (*argv, "-meeting") || !strcmp (*argv, "-mtg")) {
	       if (argc==1) {
		    ss_perror(sci_idx, 0, "No arguments supplied.");
		    return;
	       } else {
		    --argc;
		    mtg = *(++argv);
	       }
	  } else if (!strcmp (*argv, "-editor") || !strcmp(*argv, "-ed")) {
	       if (argc==1) {
		    ss_perror(sci_idx, 0, "No editor name supplied.");
		    return;
	       } else {
		    --argc;
		    editor = *(++argv);
	       }
	  } else if (!strcmp(*argv, "-no_editor") || !strcmp(*argv, "-ned")) {
	       editor = "";
	  } else {
	       (void) fprintf(stderr,
	   "Usage:  %s [ -editor cmd ] [ -no_editor ] [ -mtg meeting_name ]\n",
			      whoami);
	       return;
	  }
     }

     flag_interrupts();

     if (mtg) {
	  (void) sprintf(buffer, "goto %s", mtg);
	  code = ss_execute_line(sci_idx, buffer);
	  if (code != 0) {
	       ss_perror(sci_idx, code, buffer);
	       goto punt;
	  }
	  if (interrupt) goto punt;
     }

     if (!dsc_public.attending) {
	  ss_perror(sci_idx, DISC_NO_MTG, "");
	  goto punt;
     }

     if(!acl_is_subset("w", dsc_public.m_info.access_modes)) {
	  ss_perror(sci_idx, 0,
		    "You do not have permission to create transactions in this meeting.");
	  goto punt;
     }

     dsc_whoami(&dsc_public.nb, &myname, &code);
     if (interrupt) goto punt;
     if (code != 0) {
          ss_perror(sci_idx, code, "while checking for anonymity");
     } else {
	  if (strncmp(myname, "???", 3) == 0) {
		printf("Entry will be anonymous.\n");
	  }
     }
     free(myname);
     myname = NULL;

     (void) printf("Subject: ");
     if (fgets(subject,BUFSIZ,stdin) == (char *)NULL) {
	  clearerr(stdin);
	  if (interrupt) goto punt;
	  ss_perror(sci_idx, errno, "Error reading subject.");
	  goto punt;
     }
     if (interrupt) goto punt;
     subject[strlen(subject)-1] = '\0';		/* Get rid of NL */

     (void) unlink(temp_file);
     if ((code = edit(temp_file, editor)) != 0) {
	  ss_perror(sci_idx, code,
		    "Error during edit; transaction not entered.");
	  goto punt;
     }
     if (interrupt) goto punt;
     fd = open(temp_file, O_RDONLY, 0);
     if (fd < 0) {
	  ss_perror(sci_idx, errno, "Can't read transaction");
	  goto punt;
     }
     tf = unix_tfile(fd);
     dsc_add_trn(&dsc_public.nb, tf, subject, 0,
		 &txn_no, &code);
     if (code != 0) {
	  ss_perror(sci_idx, code, "while adding transaction");
	  close(fd);
	  goto punt;
     }

     close(fd);
     (void) unlink(temp_file);

     (void) printf("Transaction [%04d] entered in the %s meeting.\n",
		    txn_no, dsc_public.mtg_name);

     if (dsc_public.current == 0)
	 dsc_public.current = txn_no;

     /* and now a pragmatic definition of 'seen':  If you are up-to-date
	in a meeting, then you see transactions you enter. */
     if (dsc_public.highest_seen == txn_no -1) {
	  dsc_public.highest_seen = txn_no;
     }

     /* update last */
     dsc_public.m_info.last = txn_no;
     dsc_public.m_info.highest = txn_no;
punt:
     dont_flag_interrupts();
}
