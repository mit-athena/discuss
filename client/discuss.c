/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.31 1987-04-19 21:49:10 srz Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	A simple shell-type user interface to discuss; uses Ken Raeburn's
 *	ss library for the command interpreter.
 *
 *      $Log: not supported by cvs2svn $
 * Revision 1.30  87/04/12  08:13:38  wesommer
 * Cleaned up error message for "bad control arg".
 * 
 * Revision 1.29  87/04/08  03:54:00  wesommer
 * Added new-user setup hooks.
 * 
 * Revision 1.28  87/03/22  04:33:23  spook
 * *** empty log message ***
 * 
 * Revision 1.26  86/12/14  12:04:11  spook
 * Fix implementation of -editor, -no_editor that was breaking things
 * elsewhere...
 * 
 * Revision 1.25  86/12/08  00:43:30  wesommer
 * Implemented -editor, -no_editor control args for program, 
 * similar args and -mtg arg for repl.
 * 
 * Revision 1.24  86/12/07  21:51:10  wesommer
 * Added -editor and -no_editor control args to permit use under emacs.
 * 
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
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.31 1987-04-19 21:49:10 srz Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include <pwd.h>
#include <ctype.h>
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

char	dsc_version [] = "1.2";
char	*temp_file = (char *)NULL;
char	*pgm = (char *)NULL;
char	buf[BUFSIZ];
char	*buffer = &buf[0];
int	sci_idx;
char	*user_id = (char *)NULL;

/* EXTERNAL ROUTINES */

char	*malloc(), *getenv(), *gets(), *ctime();
tfile	unix_tfile();
char	*local_realm();

#define DEFAULT_EDITOR "/bin/ed"

main(argc, argv)
	int argc;
	char **argv;
{
	int code;
	char *initial_meeting = (char *)NULL;
	char *subsystem_name = "discuss";
	char *argv0 = argv[0];
	char *initial_request = (char *)NULL;
	bool quit = FALSE;	/* quit after processing request */
	bool flame = FALSE;	/* Have we flamed them for multiple  */

	editor_path = getenv ("EDITOR");
	if (!editor_path)
		editor_path = DEFAULT_EDITOR;

	while (++argv, --argc) {
		if (!strcmp(*argv, "-prompt")) {
			if (argc == 1) {
				fprintf(stderr,
					"No argument supplied with -prompt\n");
				exit(1);
			}
			argc--; argv++;
			subsystem_name = *argv;
		}
		else if (!strcmp(*argv, "-request") || !strcmp(*argv, "-rq")) {
			if (argc == 1) {
				fprintf(stderr,
					"No string supplied with -request.\n");
				exit(1);
			}
			argc--; argv++;
			initial_request = *argv;
		}
		else if (!strcmp(*argv, "-quit"))
			quit = TRUE;
		else if (!strcmp(*argv, "-no_quit"))
			quit = FALSE;
		else if (!strcmp(*argv, "-editor")) {
			if (argc == 1) {
				fprintf(stderr, "No editor name supplied with -editor\n");
				exit(1);
			}
			if (!use_editor) { 
				fprintf(stderr, "Both -editor and -no_editor specified\n");
				exit(1);
			}
			--argc;
			editor_path = *(++argv);
		}
		else if (!strcmp(*argv, "-no_editor"))
			use_editor = FALSE;
		else if (**argv == '-') {
			fprintf(stderr, "Unknown control argument %s\n",
				*argv);
			fprintf(stderr, "Usage: %s [ -prompt name ] [ -request name ] [ -quit ]\n\t\t[ -editor editor_path ] [ -no_editor ]\n",
				argv0);
			exit(1);
		}
		else {
			if (initial_meeting) {
				if(!flame) {
					fprintf(stderr, 
"More than one meeting name supplied on command line; using %s\n", 
						initial_meeting);
					flame = TRUE;
				}
			} else initial_meeting = *argv;
		}
	}

	if (!user_id) {
		register char *user = getpwuid(getuid())->pw_name;
		register char *realm = local_realm();
		register char *uid = malloc((unsigned)
					    (strlen(user)+strlen(realm)+2));
		strcpy(uid, user);
		strcat(uid, "@");
		strcat(uid, realm);
		user_id = uid;
	}

	sci_idx = ss_create_invocation(subsystem_name, dsc_version,
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

	if (code = find_rc_filename()) {
		register char *prompt;
		ss_perror(sci_idx, code, "");
		fprintf(stderr, "\n\
If you are using discuss for the first time, or if you have only used the\n\
experimental version of discuss, you need to run the 'dsc_setup'\n\
command from the shell.\n\n");
		fflush(stderr);
		prompt = "Run dsc_setup now? (y or n) ";
		while (getyn(prompt, 'y')) {
			printf("\nRunning setup...\n");
			system("dsc_setup");
			if (code = find_rc_filename()) {
				ss_perror(sci_idx, code, "");
				prompt = 
		  "\nThat didn't seem to work; try again? (y or n)";
			} else break;
		}
	}
	if (code) log_warning(code, "- continuing anyway");

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

int
getyn(prompt,def)
char *prompt,def;
{
	char yn_inp[128];

	for (;;) {
		(void) printf("%s ",prompt);
		(void) gets(yn_inp);
		if (yn_inp[0] == '\0')
			yn_inp[0] = def;
		if (toupper(yn_inp[0]) == 'Y' || toupper(yn_inp[0]) == 'N')
			return (toupper(yn_inp[0]) == 'Y');
		printf("Please enter 'Yes' or 'No'\n\n");
	}
}
