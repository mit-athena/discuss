/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/new_trans.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/new_trans.c,v 1.11 1987-07-07 21:59:18 wesommer Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	New-transaction routine for DISCUSS.  (Request 'talk'.)
 *
 */


#ifndef lint
static char rcsid_discuss_c[] =
     "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/new_trans.c,v 1.11 1987-07-07 21:59:18 wesommer Exp $";
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
#include "globals.h"
#include "acl.h"
#include "discuss_err.h"

#ifdef	lint
#define	USE(var)	var=var;
#else	lint
#define	USE(var)	;
#endif	lint

extern tfile	unix_tfile();
extern char *gets(), *error_message();

new_trans(argc, argv)
     int argc;
     char **argv;
{
     int fd, txn_no;
     tfile tf;
     char *subject = &buffer[0];
     char *whoami = argv[0];
     char *mtg = NULL;
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
	  } else if (!strcmp(*argv, "-no_editor")) {
	       editor = "";
	  } else {
	       (void) fprintf(stderr,
	   "Usage:  %s [ -editor cmd ] [ -no_editor ] [ -mtg meeting_name ]\n",
			      whoami);
	       return;
	  }
     }
	
     if (mtg) {
	  (void) sprintf(buffer, "goto %s", mtg);
	  ss_execute_line(sci_idx, buffer, &code);
	  if (code != 0) {
	       ss_perror(sci_idx, code, buffer);
	       return;
	  }
     }

     if (!dsc_public.attending) {
	  ss_perror(sci_idx, DISC_NO_MTG, "");
	  return;
     }

     /*
      * Sanity check on access control; this could be changed on the fly
      * (which is why it is only a warning)
      */
     if(!acl_is_subset("w", dsc_public.m_info.access_modes))
	  ss_perror(sci_idx, 0,
		    "Warning: meeting is read-only (enter will fail).\n");

     (void) printf("Subject: ");
     if (gets(subject) == (char *)NULL) {
	  ss_perror(sci_idx, errno, "Error reading subject.");
	  clearerr(stdin);
	  return;
     }
     (void) unlink(temp_file);
     if ((code = edit(temp_file, editor)) != 0) {
	  ss_perror(sci_idx, code,
		    "Error during edit; transaction not entered.");
	  unlink(temp_file);
	  return;
     }
     fd = open(temp_file, O_RDONLY, 0);
     if (fd < 0) {
	  ss_perror(sci_idx, errno, "Can't read transaction");
	  return;
     }
     tf = unix_tfile(fd);
     dsc_add_trn(&dsc_public.nb, tf, subject, 0,
		 &txn_no, &code);
     if (code != 0) {
	  ss_perror(sci_idx, errno, "Error adding transaction");
	  return;
     }
     (void) printf("Transaction [%04d] entered in the %s meeting.\n",
		   txn_no, dsc_public.mtg_name);
     if (dsc_public.current == 0)
	  dsc_public.current = txn_no;

     /* and now a pragmatic definition of 'seen':  If you are up-to-date
	in a meeting, then you see transactions you enter. */
     if (dsc_public.highest_seen == txn_no -1) {
	  dsc_public.highest_seen = txn_no;
     }
}
