/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v 1.2 1986-12-07 19:32:06 wesommer Exp $
 *	$Locker:  $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board.
 *	
 *	Utility routines.
 *
 *	$Log: not supported by cvs2svn $
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
static char *rcsid_discuss_utils_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/edit.c,v 1.2 1986-12-07 19:32:06 wesommer Exp $";
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

#define	DEFAULT_EDITOR	"/bin/ed"

/*
 * int edit(fn)
 *
 * fn: pathname of file to edit
 *
 * return value: error_code if error occurs, or child exits with nonzero
 *	status, 0 otherwise
 *
 * call up an editor (from environment variable EDITOR or default
 *	value DEFAULT_EDITOR) on the specified file.
 */

int
edit(fn)
	char *fn;
{
	int pid;
	char *ed = (char *)getenv("EDITOR");
	int (*handler)();
	union wait wbuf;

	if (!ed)
		ed = DEFAULT_EDITOR;

	creat(fn, 0600);
	switch ((pid = fork())) {
	case -1:
		perror("couldn't fork");
		return(errno);
	case 0:
		(void) execlp(ed, ed, fn, 0);
		(void) perror(ed);
		exit(1);
		break;
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
	return(0);
}
