/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v 1.12 1993-04-28 11:17:05 miki Exp $
 *	$Locker:  $
 *
 *	Utility routines.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.11  89/06/02  23:37:01  srz
 * Added standard copyright notice.
 * 
 * Revision 1.10  89/03/26  23:20:08  raeburn
 * Commented out extra text after #endif directive
 * 
 * Revision 1.9  89/01/24  19:34:14  srz
 * Protect the temporary file used in editing.
 * Mode 0700.
 * 
 * Revision 1.8  89/01/05  00:17:56  raeburn
 * replaced included header files with <discuss/discuss.h>
 * 
 * Revision 1.7  88/04/03  21:55:11  srz
 * Added check for interrupt in edit loop, so that won't have to
 * end transaction to have ^C work.
 * 
 * Revision 1.6  88/02/07  23:09:48  balamac
 * Added Fend options to the type-in prompter
 * 
 * Revision 1.5  86/12/14  12:02:53  spook
 * Fixed -editor/-no_editor so that it doesn't break other things..
 * 
 * Revision 1.4  86/12/08  00:44:09  wesommer
 * Added simple "line editor" function; changed calling sequence.
 * 
 * Revision 1.3  86/12/07  21:51:37  wesommer
 * Added -editor and -no_editor control args to permit use under emacs.
 * 
 * Revision 1.2  86/12/07  19:32:06  wesommer
 * [spook] Create file before entering.
 * 
 * Revision 1.1  86/12/07  01:30:14  rfrench
 * Initial revision
 * 
 * Revision 1.10  86/12/07  00:39:01  rfrench
 * Killed ../include
 * 
 * Revision 1.9  86/11/11  16:32:44  spook
 * Fixed to work with changes in et stuff
 * 
 * Revision 1.8  86/10/27  16:48:15  wesommer
 * Added form-feeds after each transaction.
 * 
 * Revision 1.7  86/10/27  16:29:04  wesommer
 * Damnit, folks, use RCS.  
 * 
 * Revision 1.6  86/10/19  09:58:52  spook
 * Changed to use dsc_ routines; eliminate refs to rpc.
 * 
 * Revision 1.5  86/09/13  20:31:56  srz
 * Include file fix
 * 
 * Revision 1.4  86/08/22  00:20:38  spook
 * new error-table stuff; separated routines
 * 
 * Revision 1.3  86/08/01  02:41:59  spook
 * Moved edit() from discuss.c; made edit() ignore SIGINT while waiting
 * for editor process to exit.
 * 
 *
 */

#ifndef lint
static char rcsid_discuss_utils_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v 1.12 1993-04-28 11:17:05 miki Exp $";
#endif /* lint */

#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <signal.h>
#include "ss.h"
#include <discuss/discuss.h>
#include "globals.h"
#include <sys/wait.h>
#include <sys/types.h>
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
#ifndef SOLARIS
	union wait wbuf;
#else
	int wbuf;
#endif
	struct stat buf;
	char buffer[BUFSIZ];
	FILE *the_file = NULL;

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
			if ((gets(buffer) == NULL) || interrupt || !strcmp(buffer, ".")) break;
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
		handler = signal(SIGINT, SIG_IGN);
		while (wait(&wbuf) != pid)
			;
		(void) signal(SIGINT, handler);
		if (WIFSIGNALED(wbuf))
			return(ET_CHILD_DIED);
#ifndef SOLARIS
		if (wbuf.w_retcode != 0)
#else
		if (wbuf != 0)
#endif
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
