/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list_acl.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list_acl.c,v 1.11 1994-03-25 16:33:45 miki Exp $
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.10  89/06/02  23:37:37  srz
 * Added standard copyright notice.
 * 
 * Revision 1.9  89/01/05  01:20:31  raeburn
 * replaced included header files with <discuss/discuss.h>
 * 
 * Revision 1.8  88/08/10  21:15:03  raeburn
 * Changed type names of acl code.
 * 
 * Revision 1.7  88/02/15  00:58:51  wesommer
 * Saber/lint fixes.
 * 
 * Revision 1.6  87/03/24  14:02:47  spook
 * Changes for new interfaces..
 * 
 * Revision 1.5  87/02/21  20:11:08  wesommer
 * Major changes:
 * 	Fix bug introduced by KR (la USER, not la MEETING; the current
 * meeting is always implied).
 *      Changed to default realm to local_realm if not present.
 *      da can now take multiple principal names.
 * 
 * Revision 1.4  86/12/07  16:04:45  rfrench
 * Globalized sci_idx
 * 
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
static char rcsid_list_acl_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list_acl.c,v 1.11 1994-03-25 16:33:45 miki Exp $";
#endif lint

#include <string.h>
#include <stdio.h>
#include <discuss/discuss.h>
#include "globals.h"

extern char *malloc(), *error_message(), *local_realm();

list_acl(argc, argv)
	int argc;
	char **argv;
{
	dsc_acl *list;
	int code;
	char *modes;

	if (!dsc_public.attending) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	/*
	 * List access to current meeting.
	 */

	if (argc == 1) {
		/* long form; dump the access control list */
		dsc_get_acl(&dsc_public.nb, &code, &list);
		if (code) {
			fprintf(stderr, "Can't read ACL: %s\n", 
				error_message(code));
			return;
		}
		print_acl(list);
		acl_destroy(list);
	}
	
	while(++argv,--argc) {
		dsc_get_access(&dsc_public.nb, *argv, &modes, &code);
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
	dsc_acl *list;
{
	register dsc_acl_entry *ae;
	register int n;
	for (ae = list->acl_entries, n = list->acl_length; n; --n, ++ae)
		printf("%-10s %s\n", ae->modes, ae->principal);
}

static char buf[120];

set_acl(argc, argv)
     int argc;
     register char **argv;
{
	register char *modes, *principal;
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

	if (strcmp(argv[2],"*") != 0 && strchr(argv[2], '@') == 0) {
		strcpy(buf, argv[2]);
		strcat(buf, "@");
		strcat(buf, local_realm());
		principal = buf;
	} else principal=argv[2];

	dsc_set_access(&dsc_public.nb, principal, modes, &code);
	if(code) fprintf(stderr, "Can't set access for %s: %s\n",
			 principal, error_message(code));
}

del_acl(argc, argv)
	int argc;
	register char **argv;
{
	int code;
	register char *principal;

	if (!dsc_public.attending) {
		(void) fprintf(stderr, "No current meeting.\n");
		return;
	}
	if (argc < 2) {
		printf("usage: %s principal [ , principal ... ]\n", *argv);
		return;
	}

        while(++argv,--argc) {
		if (strcmp(*argv,"*") != 0 && strchr(*argv, '@') == 0) {
			strcpy(buf, *argv);
			strcat(buf, "@");
			strcat(buf, local_realm());
			principal = buf;
		} else principal = *argv;
		dsc_delete_access(&dsc_public.nb, principal, &code);
		if(code) fprintf(stderr, "Can't delete access of %s: %s\n",
				 principal, error_message(code));
	}
}
