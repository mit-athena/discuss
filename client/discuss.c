/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.24 1986-12-07 21:51:10 wesommer Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	A simple shell-type user interface to discuss; uses Ken Raeburn's
 *	ss library for the command interpreter.
 *
 *      $Log: not supported by cvs2svn $
 * Revision 1.23  86/12/07  17:49:30  wesommer
 * Lint fixes.
 * 
 * Revision 1.22  86/12/07  16:04:23  rfrench
 * Globalized sci_idx
 * 
 * Revision 1.21  86/12/07  00:29:25  rfrench
 * Killed ../include
 * 
 * Revision 1.20  86/11/20  10:32:10  srz
 * Reply sets current right
 * 
 * Revision 1.19  86/11/17  00:58:05  spook
 * Added some control arg processing.  (-ssn, -rq, -quit)
 * 
 * Revision 1.18  86/11/16  06:16:09  wesommer
 * Added call to init_acl_err_tbl.
 * 
 * Revision 1.17  86/11/11  01:49:11  wesommer
 * Added access control warning on reply.
 * Added access control flags on entry to meetings: chairmen are told
 * such, and if reply or write access are lacking, the user is told
 * "read-only", "reply-only" or "no replies".
 * 
 * Revision 1.16  86/10/29  10:25:23  srz
 * Clean up global variables.
 * Moves delete and retrieve over to list.c
 * Added leave, and have goto/leave record meeting changes
 * Reply takes transaction number.
 * 
 * Revision 1.15  86/10/19  09:58:03  spook
 * Changed to use dsc_ routines; eliminate refs to rpc.
 * 
 * Revision 1.14  86/09/16  21:52:10  wesommer
 * Close off RPC connections so we don't lose file descriptors.
 * 
 * Revision 1.13  86/09/16  21:33:42  srz
 * bug fixes of last checkout.
 * 
 * Revision 1.12  86/09/13  20:41:42  srz
 * Added name resolving to goto_mtg
 * 
 * Revision 1.11  86/09/10  18:57:03  wesommer
 * Made to work with kerberos; meeting names are now longer.
 * ./
 * 
 * Revision 1.10  86/09/10  17:20:11  wesommer
 * Ken, please use RCS..
 * 
 * Revision 1.9  86/08/23  21:42:48  spook
 * moved timecheck for list into list module
 * 
 * Revision 1.8  86/08/22  00:19:19  spook
 * using new error-table stuff; moved some code out to other
 * modules
 * 
 * Revision 1.7  86/08/07  13:40:44  spook
 * replaced "/projects/discuss/client/info" with #define from config.h
 * 
 * Revision 1.6  86/08/02  14:01:11  wesommer
 * Fixed to ignore SIGPIPE if the pager goes away.
 * 
 * Revision 1.5  86/08/01  02:41:35  spook
 * Moved edit() to discuss_utils.c.
 * 
 * Revision 1.4  86/07/31  15:56:08  wesommer
 * Fixed up some brain-damage surrounding the prt_trans/write_trans
 * interactions.
 *      "If you're using longjmp, you're doing something wrong"
 *                              - Jim Gettys
 * write_trans no longer takes an sci_idx as its argument; it has an
 * additional last argument, which is an error code.
 * 
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.24 1986-12-07 21:51:10 wesommer Exp $";
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

/* GLOBAL VARIABLES */

struct _dsc_pub dsc_public = {0, 0, 0, (char *)NULL, (char *)NULL };

char	*temp_file = (char *)NULL;
char	*pgm = (char *)NULL;
char	buf[BUFSIZ];
char	*buffer = &buf[0];
int	sci_idx;
bool	use_editor = TRUE;

/* EXTERNAL ROUTINES */

char	*malloc(), *getenv(), *gets(), *ctime();
tfile	unix_tfile();

main(argc, argv)
	int argc;
	char **argv;
{
	int code;
	char *initial_meeting = (char *)NULL;
	char *subsystem_name = "discuss";
	char *initial_request = (char *)NULL;
	int quit = 0;		/* quit after processing request */

	while (++argv, --argc) {
		if (!strcmp(*argv, "-subsystem_name") || !strcmp(*argv, "-ssn")) {
			if (argc == 1) {
				fprintf(stderr, "No argument supplied with -subsystem_name\n");
				exit(1);
			}
			argc--; argv++;
			subsystem_name = *argv;
		}
		else if (!strcmp(*argv, "-request") || !strcmp(*argv, "-rq")) {
			if (argc == 1) {
				fprintf(stderr, "No string supplied with -request.\n");
				exit(1);
			}
			argc--; argv++;
			initial_request = *argv;
		}
		else if (!strcmp(*argv, "-quit"))
			quit = 1;
		else if (!strcmp(*argv, "-no_quit"))
			quit = 0;
		else if (!strcmp(*argv, "-editor"))
			use_editor = TRUE;
		else if (!strcmp(*argv, "-no_editor"))
			use_editor = FALSE;
		else if (**argv == '-') {
			fprintf(stderr, "Unknown control argument %s\n",
				*argv);
			exit(1);
		}
		else
			initial_meeting = *argv;
	}

	sci_idx = ss_create_invocation(subsystem_name, CURRENT_VERSION,
				       (char *)NULL, &discuss_cmds, &code);
	if (code) {
		ss_perror(sci_idx, code, "creating invocation");
		exit(1);
	}
	(void) ss_add_info_dir(sci_idx, INFO_DIR, &code);
	if (code) {
		ss_perror(sci_idx, code, INFO_DIR);
	}

	init_disc_err_tbl();
	init_dsc_err_tbl();

	temp_file = malloc(64);
	pgm = malloc(64);
	(void) sprintf(temp_file, "/tmp/mtg%d.%d", (int)getuid(), getpid());

	if (initial_meeting != (char *)NULL) {
		(void) sprintf(buffer, "goto %s", initial_meeting);
		ss_execute_line(sci_idx, buffer, &code);
		if (code != 0)
			ss_perror(sci_idx, code, initial_meeting);
	}
	if (initial_request != (char *)NULL) {
		(void) ss_execute_line(sci_idx, initial_request, &code);
		if (code != 0)
			ss_perror(sci_idx, code, initial_request);
	}
	if (!quit || code)
		(void) ss_listen (sci_idx, &code);
	(void) unlink(temp_file);
	leave_mtg();				/* clean up after ourselves */
}



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

	if (!dsc_public.attending) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		return;
	}

	if (argc > 2) {
		(void) fprintf(stderr, "Usage:  %s\n", argv[0]);
		return;
	}
	dsc_get_trn_info(dsc_public.mtg_uid, dsc_public.current, &t_info, &code);
	if (code != 0)
		t_info.current = 0;
	else {
	     free(t_info.subject);			/* don't need these */
	     t_info.subject = NULL;
	     free(t_info.author);
	     t_info.author = NULL;
	}

	trn_list = trn_select(&t_info, (argc == 1) ? "current" : argv[1],
			      (selection_list *)NULL, &code);
	if (code) {
	     ss_perror(sci_idx, code, "");
	     free((char *) trn_list);
	     return;
	}

	if (trn_list -> low != trn_list -> high) {
	     ss_perror(sci_idx, 0, "Cannot reply to range");
	     free((char *)trn_list);
	     return;
	}

	orig_trn = trn_list -> low;
	free((char *)trn_list);

	dsc_get_trn_info(dsc_public.mtg_uid, orig_trn, &t_info, &code);
	if (code != 0) {
	     ss_perror(sci_idx, code, "");
	     return;
	}

	if(!acl_is_subset("a", dsc_public.m_info.access_modes))
		(void) fprintf(stderr, "Warning: You do not have permission to create replies.\n");

	if (strncmp(t_info.subject, "Re: ", 4)) {
		char *new_subject = malloc((unsigned)strlen(t_info.subject)+5);
		(void) strcpy(new_subject, "Re: ");
		(void) strcat(new_subject, t_info.subject);
		(void) free(t_info.subject);
		t_info.subject = new_subject;
	}

	(void) unlink(temp_file);
	if (edit(temp_file) != 0) {
		(void) fprintf(stderr,
			       "Error during edit; transaction not entered\n");
		unlink(temp_file);
		goto abort;
	}
	fd = open(temp_file, O_RDONLY, 0);
	if (fd < 0) {
		(void) fprintf(stderr, "No file; not entered.\n");
		goto abort;
	}
	tf = unix_tfile(fd);
	
	dsc_add_trn(dsc_public.mtg_uid, tf, t_info.subject,
		orig_trn, &txn_no, &code);
	if (code != 0) {
		fprintf(stderr, "Error adding transaction: %s\n",
			error_message(code));
		goto abort;
	}
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		      txn_no, dsc_public.mtg_name);

	dsc_public.current = orig_trn;

	/* and now a pragmatic definition of 'seen':  If you are up-to-date
	   in a meeting, then you see transactions you enter. */
	if (dsc_public.highest_seen == txn_no -1) {
	     dsc_public.highest_seen = txn_no;
	}

abort:
	free(t_info.subject);
	free(t_info.author);
}

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

	dsc_public.mtg_name = (char *)malloc(strlen(argv[1])+1);
	strcpy(dsc_public.mtg_name, argv[1]);

	get_mtg_unique_id ("", "", dsc_public.mtg_name, &dsc_public.nb, &code);
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

     dsc_public.nb.date_attended = time((long *)0);
     dsc_public.nb.last = dsc_public.highest_seen;
     update_mtg_set ("", "", &dsc_public.nb, 1, &code);

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
