/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.54 1996-09-08 20:31:10 ghudson Exp $
 *	$Locker:  $
 *
 *	A simple shell-type user interface to discuss; uses Ken Raeburn's
 *	ss library for the command interpreter.
 *
 */


#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/discuss.c,v 1.54 1996-09-08 20:31:10 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <pwd.h>
#include <ss/ss.h>
#include <discuss/discuss.h>
#include "config.h"
#include "globals.h"

#ifdef	lint
#define	DONT_USE(var)	var=var;
#else	lint
#define	DONT_USE(var)	;
#endif	lint

#define	FREE(ptr)	{ if (ptr) free(ptr); }
#define max(a, b) ((a) > (b) ? (a) : (b))

extern ss_request_table discuss_cmds;

char	dsc_version[] = "1.6";
int	sci_idx;

extern char *temp_file, *pgm, *user_id;

/* EXTERNAL ROUTINES */

char	*malloc(), *getenv(), *gets(), *ctime();
tfile	unix_tfile();
char	*local_realm();
static char	buf[BUFSIZ];
char	*buffer = buf;

int main (argc, argv)
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

	signal(SIGPIPE, SIG_IGN);
	editor_path = getenv ("DISCUSS_EDITOR");

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
				if (!flame) {
					fprintf(stderr, 
"More than one meeting name supplied on command line; using %s\n", 
						initial_meeting);
					flame = TRUE;
				}
			} else initial_meeting = *argv;
		}
	}

	{
		char *user;
		struct passwd *user_pw = getpwuid(getuid());
		register char *realm = local_realm();
		

		if (user_pw == NULL) {
		     fprintf(stderr,
		 "You do not appear in /etc/passwd.  Cannot continue.\n");
		     exit(1);
		}
		user = user_pw -> pw_name;
		user_id = malloc((unsigned)(strlen(user)+strlen(realm)+2));
		strcpy(user_id, user);
		strcat(user_id, "@");
		strcat(user_id, realm);
	}

	sci_idx = ss_create_invocation(subsystem_name, dsc_version,
				       (char *)NULL, &discuss_cmds, &code);
	if (code) {
	    com_err (subsystem_name, code, "creating invocation");
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
		  printf("\nRunning dsc_setup...\n");
		  system("dsc_setup");
		  if (code = find_rc_filename()) {
		       ss_perror(sci_idx, code, "");
		       prompt =
			    "\nThat didn't seem to work; try again? (y or n)";
		  } else break;
	     }
	     if (code)
		 exit (1);
	}
	else if (!quit) {
	     printf("Discuss version %s.  Type '?' for a list of commands.\n",
		    dsc_version);
	     if (!initial_meeting)
		  printf("\n");
	}

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
	leave_mtg();				/* clean up after ourselves */
	return 0;
}

int getyn(prompt,def)
    char *prompt, def;
{
	char inp[128];

	for (;;) {
		(void) printf("%s ",prompt);
		if (fgets (inp, 128, stdin) == NULL)
		    return FALSE;
		else if (inp[0] == '\n')
		    inp[0] = def;
		if (inp[0] == 'y' || inp[0] == 'Y')
		    return 1;
		else if (inp[0] == 'n' || inp[0] == 'N')
		    return 0;
		printf("Please enter 'Yes' or 'No'\n\n");
	}
}
