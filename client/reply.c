/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/reply.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/reply.c,v 1.19 1996-09-08 20:31:31 ghudson Exp $
 *	$Locker:  $
 *
 *	Code for "reply" request in discuss.
 *
 */


#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/reply.c,v 1.19 1996-09-08 20:31:31 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#ifndef SOLARIS
#include <strings.h>
#else
#include <string.h>
#include <fcntl.h>
#endif
#include <sys/wait.h>
#include <ss/ss.h>
#include <discuss/discuss.h>
#include "config.h"
#include "globals.h"

/* EXTERNAL ROUTINES */

char	*malloc(), *getenv(), *gets(), *ctime(), *error_message();
tfile	unix_tfile();
extern void flag_interrupts(),dont_flag_interrupts();

#define DEFAULT_EDITOR "/bin/ed"

repl(argc, argv)
	int argc;
	char **argv;
{
	int fd;
	trn_nums txn_no, orig_trn;
	tfile tf;
	selection_list *trn_list;
	trn_info t_info;
	int code;
	int prompt_subject = FALSE;
	char *subject = &buffer[0];
	char *editor = NULL;
	char *trans = NULL;
	char *mtg = NULL;
	char *myname = NULL;

	while (++argv, --argc) {
		if (!strcmp (*argv, "-meeting") || !strcmp (*argv, "-mtg")) {
			if (argc==1) {
				(void) fprintf(stderr, 
					       "No argument to %s.\n", *argv);
				return;
			} else {
				--argc;
				mtg = *(++argv);
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
			editor = "";
		} else if (!strcmp(*argv, "-subject") ||
			   !strcmp(*argv, "-sj")) {
		    /* prompt for subject */   
		    prompt_subject = TRUE;
		} else {
			if (!trans) trans = *argv; 
			else {
			    ss_perror(sci_idx, 0,
				      "Cannot reply to multiple transactions");
			    return; 
			}
		}
	}
	if (mtg && !trans) { 
		fprintf(stderr,
			"Must have transaction specifier if using -mtg.\n");
		return;
	}
	if (mtg) {
		(void) sprintf(buffer, "goto %s", mtg);
		ss_execute_line(sci_idx, buffer, &code);
		if (code != 0) {
			ss_perror(sci_idx, code, buffer);
			return;
		}
	}

	flag_interrupts();
	if (!dsc_public.attending) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		goto abort2;
	}

	dsc_get_trn_info(&dsc_public.nb, dsc_public.current, &t_info, &code);
	if (code != 0)
		t_info.current = 0;
	dsc_destroy_trn_info(&t_info);

	trn_list = trn_select(&t_info, trans ? trans : "current" ,
			      (selection_list *)NULL, &code);
	if (code) {
	     ss_perror(sci_idx, code, "");
	     sl_free((char *) trn_list);
	     goto abort2;
	}

	if (trn_list -> low != trn_list -> high) {
	     ss_perror(sci_idx, 0, "Cannot reply to range");
	     sl_free((char *)trn_list);
	     goto abort2;
	}

	orig_trn = trn_list -> low;
	sl_free((char *)trn_list);

	dsc_get_trn_info(&dsc_public.nb, orig_trn, &t_info, &code);

	if (code != 0) {
		ss_perror(sci_idx, code, "");
		goto abort;
	}

	if(!acl_is_subset("a", dsc_public.m_info.access_modes)) {
	    (void) ss_perror(sci_idx, 0,
			     "You do not have permission to create replies in this meeting.");
	    goto abort;
	}

	dsc_whoami(&dsc_public.nb, &myname, &code);

	if (code != 0) {
		ss_perror(sci_idx, code, "while checking for anonymity");
	} else {
		if (strncmp(myname, "???", 3) == 0) {
			printf("Reply will be anonymous.\n");
		}
	}
	free(myname);
	myname = NULL;
	
	if (prompt_subject) {
	    (void) printf("Subject: ");
	    if (fgets(subject,BUFSIZ,stdin) == (char *)NULL) {
		clearerr(stdin);
		if (interrupt) goto abort;
		ss_perror(sci_idx, errno, "Error reading subject.");
		goto abort;
	    }
	    if (interrupt) goto abort;
	    subject[strlen(subject)-1] = '\0';		/* Get rid of NL */
	} else {
	    if (strncmp(t_info.subject, "Re: ", 4)) {
		char *new_subject = malloc((unsigned)strlen(t_info.subject)+5);
		(void) strcpy(new_subject, "Re: ");
		(void) strcat(new_subject, t_info.subject);
		(void) free(t_info.subject);
		t_info.subject = new_subject;
	    }

	    printf("Subject: %s\n", t_info.subject);
	}
	(void) unlink(temp_file);
	if (edit(temp_file, editor) != 0) {
		(void) fprintf(stderr,
			       "Error during edit; transaction not entered\n");
		goto abort;
	}
	if (interrupt) goto abort;
	fd = open(temp_file, O_RDONLY, 0);
	if (fd < 0) {
		(void) fprintf(stderr, "No file; not entered.\n");
		goto abort;
	}
	tf = unix_tfile(fd);
	
	dsc_add_trn(&dsc_public.nb, tf,
		    prompt_subject ? subject : t_info.subject,
		    orig_trn, &txn_no, &code);
	if (code != 0) {
		ss_perror(sci_idx, code, "while adding transaction\n");
		close(fd);
		goto abort;
	}
 
	close(fd);
	(void) unlink(temp_file);
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		       txn_no, dsc_public.mtg_name);

	dsc_public.current = orig_trn;

	/* and now a pragmatic definition of 'seen':  If you are up-to-date
	   in a meeting, then you see transactions you enter. */
	if (dsc_public.highest_seen == txn_no -1) {
		dsc_public.highest_seen = txn_no;
	}

	/* Update last */
	dsc_public.m_info.last = txn_no;
	dsc_public.m_info.highest = txn_no;

abort:
	dsc_destroy_trn_info(&t_info);
abort2:
	dont_flag_interrupts();
}

