/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: edit.c,v 1.16 1999-02-02 20:39:46 kcr Exp $
 *
 *	Utility routines.
 *
 *
 */

#ifndef lint
static char rcsid_discuss_utils_c[] =
    "$Id: edit.c,v 1.16 1999-02-02 20:39:46 kcr Exp $";
#endif /* lint */

#include <stdlib.h>
#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <ss/ss.h>
#include <discuss/discuss.h>
#include "globals.h"
#include <sys/types.h>
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#include <sys/stat.h>
#include <sys/errno.h>

bool	use_editor = TRUE;
char 	*editor_path = NULL;

extern char *getenv();

/*
 * int edit(fn, editor)
 *
 * fn: pathname of file to edit
 *
 * editor:
 * 	name of editor command to run; if NULL, use the default
 * (specified on the command line or $EDITOR); if "", use a simple
 * type-in prompter.
 * 
 * return value: error_code if error occurs, or child exits with nonzero
 *	status, 0 otherwise
 *
 * call up an editor (from environment variable EDITOR or default
 *	value DEFAULT_EDITOR) on the specified file.
 */

int
edit(fn, edit_path)
	char *fn;
	char *edit_path;
{
	char *editor_path_e, *editor_path_2 = NULL;
	char *editor_path_v;
	int pid;
	int (*handler)();
	int wbuf;

	struct stat buf;
	char buffer[BUFSIZ];
	FILE *the_file = NULL;
#if HAVE_SIGACTION
        struct sigaction act, oact;
        sigemptyset(&act.sa_mask);
        act.sa_flags = 0;
#endif

	editor_path_e = getenv("EDITOR");
	if (!editor_path_e) editor_path_e = "/bin/ed";
	editor_path_v = getenv("VISUAL");
	if (!editor_path_v) editor_path_v = "/usr/ucb/vi";

	if (use_editor && editor_path && !edit_path)
	    editor_path_2 = editor_path; 
	else if (edit_path && (*edit_path != '\0'))
	    editor_path_2 = edit_path;
	else {
		the_file = fopen(fn, "w");
		if (!the_file) { 
			perror(fn);
			printf("Error opening file: %d\n",errno);
			return(errno);
		}

		ftruncate(fileno(the_file), 0);
		fchmod(fileno(the_file), 0700);
		printf("Enter transaction; end with ^D or '.' on a line by itself.\n");
		for (;;) {
			if ((fgets(buffer, sizeof(buffer), stdin) == NULL) || interrupt || !strcmp(buffer, ".")) break;
			else if (!strcmp(buffer,"\\f")) {
				editor_path_2 = editor_path_e;
				break;
			} else if (!strcmp(buffer,"~e")) {
				editor_path_2 = editor_path_e;
				break;
			} else if (!strcmp(buffer,"~v")) {
				editor_path_2 = editor_path_v;
				break;
			} else {
				fputs(buffer,the_file);
				fputc('\n',the_file);
			}
		}
	}

	if (editor_path_2) {
		if (the_file) {
			clearerr(stdin);
			fclose(the_file);
		}
		switch ((pid = fork())) {
		case -1:
			perror("couldn't fork");
			printf("Couldn't fork, error %d\n",errno);
			return(errno);
		case 0:
			(void) execlp(editor_path_2, editor_path_2, fn, 0);
			(void) perror(editor_path_2);
			exit(1);
		default:
			break;
		}
#if HAVE_SIGACTION
		act.sa_handler= (void (*)()) SIG_IGN;
		(void) sigaction(SIGINT, &act, &oact);
#else
		handler = signal(SIGINT, SIG_IGN);
#endif
		while (wait(&wbuf) != pid)
			;
#if HAVE_SIGACTION
              (void) sigaction(SIGINT, &oact, NULL);
#else
		(void) signal(SIGINT, handler);
#endif
		if (WIFSIGNALED(wbuf))
			return(ET_CHILD_DIED);
		if (WEXITSTATUS(wbuf))
			return(ET_CHILD_ERR);
	} else {
		clearerr(stdin);
		fclose(the_file);
	}

	if (stat (fn, &buf) != 0 || buf.st_size == 0) {
		unlink(fn);
	}
	return(0);
}

touch(fn)
	char *fn;
{
	int fd;
	if ((fd=creat(fn, 0600)) < 0) 
		return(errno);
	else close(fd);
	return(0);
}
