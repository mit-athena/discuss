/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v 1.5 1986-12-14 12:02:53 spook Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board.
 *	
 *	Utility routines.
 *
 *	$Log: not supported by cvs2svn $
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
static char *rcsid_discuss_utils_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v 1.5 1986-12-14 12:02:53 spook Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <strings.h>
#include <signal.h>
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"
#include "discuss_err.h"
#include "globals.h"
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>

bool	use_editor = TRUE;
char 	*editor_path = NULL;

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
	int pid;
	int (*handler)();
	int code;
	union wait wbuf;
	struct stat buf;

	if ((!use_editor && !edit_path) || (edit_path && (*edit_path == '\0'))) {
		FILE *of = fopen(fn, "w");
		char buf[BUFSIZ];
		if (!of) { 
			perror(fn);
			return(errno);
		}
		ftruncate(fileno(of), 0);
		printf("Enter transaction; end with ^D or '.' on a line by itself.\n");
		while(gets(buf) != NULL && strcmp(buf, ".")) {
			fputs(buf, of);
			fputc('\n', of);
		}
		clearerr(stdin);
		fclose(of);
	} else {
		if (!edit_path) edit_path = editor_path;
		if(code = touch(fn)) return(code);

		switch ((pid = fork())) {
		case -1:
			perror("couldn't fork");
			return(errno);
		case 0:
			(void) execlp(edit_path, edit_path, fn, 0);
			(void) perror(edit_path);
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
		if (wbuf.w_retcode != 0)
			return(ET_CHILD_ERR);
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
