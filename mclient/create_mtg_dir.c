/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 * Create a meeting directory; this program should run setuid root.
 * However, it is secure..
 */

#include <stdio.h>
#include <sys/file.h>
#include <sys/param.h>
#include <pwd.h>
#include <strings.h>
#ifdef SOLARIS
#include <unistd.h>
#include <string.h>
#endif
#ifndef	lint
static char rcsid_create_mtg_dir_c[] = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/create_mtg_dir.c,v 1.5 1994-03-25 16:48:24 miki Exp $";
#endif

main(argc, argv)
	int argc;
	char **argv;
{
	char *dir;
	char *entry;
	struct passwd *pw;

	if (argc != 2) {
		fprintf(stderr, "usage: %s directory\n", argv[0]);
		exit(1);
	}

	if((pw = getpwnam("discuss")) == NULL) {
		fprintf(stderr, "discuss: unknown user\n");
		exit(1);
	}

	dir = argv[1];
	if (dir[0] != '/') {
		fprintf(stderr, "%s: Must be absolute pathname\n", dir);
		exit(1);
	}

	if ((entry = strrchr(dir, '/')) == dir) {
		fprintf(stderr, "%s: Can't create meeting there\n", dir);
		exit(1);
	}
	*entry++ = '\0';

	/*
	 * 	Access uses real uid, rather than effective uid.
	 */
	if (access(dir, W_OK)) {
		perror(dir);
		exit(1);
	}
	if (chdir(dir)) {
		perror(dir);
		exit(1);
	}
	if (!access(entry, 0)) {
		fprintf(stderr, "%s/%s: already exists\n", dir, entry);
		exit(1);
	}
	if (mkdir(entry, 0777)) {
		fprintf(stderr, "Cannot create ");
		perror(entry);
		exit(1);
	}
	if (chown(entry, pw->pw_uid, -1)) {
		fprintf(stderr, "Cannot change ownership for ");
		perror(entry);
		exit(1);
	}
}
