/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list_acl.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list_acl.c,v 1.4 1986-12-07 16:04:45 rfrench Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.3  86/12/07  00:39:15  rfrench
 * Killed ../include
 * 
 * Revision 1.2  86/11/17  01:50:36  spook
 * Allow listing acl on meetings specified on command line even when
 * there is no current meeting.
 * 
 * Revision 1.1  86/11/16  06:16:59  wesommer
 * Initial revision
 * 
 */

#ifndef lint
static char *rcsid_list_acl_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list_acl.c,v 1.4 1986-12-07 16:04:45 rfrench Exp $";
#endif lint

#include <strings.h>
#include <stdio.h>
#include "types.h"
#include "interface.h"
#include "acl.h"
#include "globals.h"

extern char *malloc();

list_acl(argc, argv)
	int argc;
	char **argv;
{
	Acl *list;
	int code;
	char *modes;

	if (!dsc_public.attending && (argc == 1)) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	/*
	 * List access to current meeting.
	 */

	if (argc == 1) {
		/* long form; dump the access control list */
		dsc_get_acl(dsc_public.mtg_uid, &code, &list);
		if (code) {
			fprintf(stderr, "Can't read ACL: %s\n", 
				error_message(code));
			return;
		}
		print_acl(list);
		acl_destroy(list);
	}
	
	while(++argv,--argc) {
		dsc_get_access(dsc_public.mtg_uid, *argv, &modes, &code);
		if (code) {
			fprintf(stderr, "Can't read ACL for %s: %s\n",
				*argv, error_message(code));
			continue;
		} 
		printf("%-10s %s\n", modes, *argv);
		free(modes);
	}
	return;
}

print_acl(list)
	Acl *list;
{
	register acl_entry *ae;
	register int n;
	for (ae = list->acl_entries, n = list->acl_length; n; --n, ++ae)
		printf("%-10s %s\n", ae->modes, ae->principal);
}

set_acl(argc, argv)
     int argc;
     char **argv;
{
	char *modes;
	int code;
	if (!dsc_public.attending) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	if (argc != 3) {
		printf("usage: %s [ modes | null ] principal \n", argv[0]);
		return;
	}
	if(!strcmp("null", argv[1])) modes = "";
	else modes = argv[1];

	dsc_set_access(dsc_public.mtg_uid, argv[2], modes, &code);
	if(code) fprintf(stderr, "Can't set access for %s: %s\n",
			 argv[2], error_message(code));
}

del_acl(argc, argv)
	int argc;
	char **argv;
{
	int code;
	if (!dsc_public.attending) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	if (argc != 2) {
		printf("usage: %s principal\n", argv[0]);
		return;
	}
	code = 0;
	dsc_delete_access(dsc_public.mtg_uid, argv[1], &code);
	if(code) fprintf(stderr, "Can't delete access of %s: %s\n",
			 argv[1], error_message(code));
}
