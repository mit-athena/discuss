/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: mkds.c,v 1.22 1999-02-02 20:40:33 kcr Exp $
 *
 */

#ifndef lint
static char rcsid_mkds_c[] =
    "$Id: mkds.c,v 1.22 1999-02-02 20:40:33 kcr Exp $";
#endif lint

#include <discuss/discuss.h>
#if 0
#include "dsc_et.h"
#include "config.h"
#include "interface.h"
#include "rpc.h"
#include "globals.h"
#endif
#include <sys/time.h>
#include <sys/file.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <netdb.h>
#include <pwd.h>
#include <errno.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif

#define cupper(x) (islower(x)?toupper(x):(x))
#define clower(x) (isupper(x)?tolower(x):(x))

char default_dir[] = "/var/spool/discuss";
char *whoami;
int  interrupt = 0;

extern const char *local_realm();

char *strtrim();

main(argc,argv)
int argc;
char *argv[];
{
	extern tfile unix_tfile();
	name_blk nbsrc,nbdest;
	char long_name[100],short_name[100],username[50],mtg_path[100];
	char mtg_host[100];
	char *default_host;
	char temp_file[64];
	char ann_mtg[100];
	int public = 0,error = 1,result,remove=0,delmtg=0;
	int fd,txn_no;
	tfile tf;
	char hostname[256];

	init_dsc_err_tbl();

	nbsrc.user_id = malloc(132);

	(void) sprintf(temp_file,"/tmp/mtg%d.%d",getuid(),getpid());

	whoami = strrchr(argv[0],'/');
	if (whoami)
		whoami++;
	else
		whoami = argv[0];

	if (argc > 1) {
		fprintf(stderr,"Usage: %s\n",whoami);
		exit (1);
	}

	if (!strcmp(whoami,"rmds"))
		remove++;
	else if (strcmp(whoami, "mkds")) {
		fprintf(stderr,
			"This program must be run as 'mkds' or 'rmds'.\n");
		exit(1);
	}

	gethostname(hostname, 256);
	{
		struct hostent *hp;
		char *h;
		hp = gethostbyname(hostname);
		h = (hp ? hp->h_name : hostname);
		default_host = malloc(strlen(h)+1);
		strcpy(default_host, h);
	}
	printf("Meeting host: [default %s]: ", default_host);
	if (!gets(mtg_host))
		exit(1);
	strcpy(mtg_host, strtrim(mtg_host));
	if (mtg_host[0] == '\0')
		strcpy(mtg_host, default_host);
	if (mtg_host[0] == '%')
		strcpy(mtg_host, "");
	else {
		struct hostent *hp;
		hp = gethostbyname(mtg_host);
		if (!hp) {
			fprintf(stderr, "Unknown host %s\n", mtg_host);
			exit(1);
		}
		strcpy(mtg_host, hp->h_name);
	}
	printf("\nMeeting location [default %s]: ", default_dir);
	if (!gets(mtg_path))
	        exit(1);
	strcpy(mtg_path, strtrim(mtg_path));
	if (!mtg_path[0])
		strcpy(mtg_path, default_dir);
	if (!remove) {
		printf("\nLong meeting name: ");
		if (!gets(long_name))
		        exit(1);
		strcpy(long_name, strtrim(long_name));
		if (long_name[0] == '\0') {
			printf("No long meeting name supplied.\n");
			exit(1);
		}
	}
	printf("\nShort meeting name: ");
	if (!gets(short_name))
	        exit(1);
	strcpy(short_name, strtrim(short_name));
	if(short_name[0] == '\0') {
		printf("No short meeting name supplied.\n");
		exit(1);
	}
	
	(void) strcpy(mtg_path, strtrim(mtg_path));
	(void) strcat(mtg_path,"/");
	(void) strcat(mtg_path, short_name);
	nbsrc.pathname = malloc(strlen(mtg_path) + 1);
	strcpy(nbsrc.pathname, mtg_path);

	nbsrc.hostname = malloc(strlen(mtg_host) + 1);
	strcpy(nbsrc.hostname, mtg_host);

	(void) strcpy (username, getpwuid(getuid())->pw_name);
	(void) strcpy (nbsrc.user_id, username);

	if (remove) {
		dsc_remove_mtg(&nbsrc,&result);
		if (result)
			(void) fprintf(stderr,"Can't remove meeting: %s\n",
				       error_message(result));
		error = result;
		goto kaboom;
	}

	printf("\n");
	public = getyn("Should this meeting be public [y]? ",'Y');

	dsc_create_mtg(mtg_host, mtg_path, long_name, public, 0,
		       &result);
	if (result) {
		if (result == ECONNREFUSED)
			fprintf(stderr, "%s doesn't appear to be running a discuss server", mtg_host);
		else fprintf(stderr, "%s.  Can't create meeting.\n",
			     error_message(result));
		goto kaboom;
	}

	if (!public &&
	    getyn("Should specified users be allowed to participate? [y]", 'Y')) {
		char username[140];
		
		printf("Enter the usernames you wish to participate; \n\
End with . on a line by itself\n\n");
		for (;;) {
			printf("User name: ");
			fflush(stdout);
			if (!gets(username)) break;
			strcpy(username, strtrim(username));
			if (strcmp(username, ".") == 0) break;
			if (strcmp(username,"*") != 0 &&
			    strchr(username, '@') == 0) {
				strcat(username, "@");
				strcat(username, local_realm());
			}
			dsc_set_access (&nbsrc, username, "aorsw", &result);
			if (result) {
				fprintf (stderr, "Can't add participant: %s\n",
					 error_message(result));
			}
		}
	}
	clearerr(stdin);
	
	delmtg = 1;

	nbsrc.date_attended = time(0);
	nbsrc.last = 0;
	nbsrc.status = 0;
	nbsrc.aliases = (char **) calloc(3, sizeof(char *));
	nbsrc.aliases[0] = malloc(strlen(long_name)+1);
	strcpy(nbsrc.aliases[0], long_name);
	nbsrc.aliases[1] = malloc(strlen(short_name)+1);
	strcpy(nbsrc.aliases[1], short_name);
	nbsrc.aliases[2] = (char *)NULL;

	nbsrc.spare = "";
		
	dsc_update_mtg_set(username,&nbsrc,1,&result);
	if (result) {
		fprintf(stderr, "mkds: Can't set meeting name: %s",
			error_message(result));
		goto kaboom;
	}

	printf("\nYou must now enter the initial transaction.\n");
	printf(
	  "This transaction will serve as an introduction to the meeting.\n");

	(void) unlink(temp_file);

	if (edit(temp_file, getenv("EDITOR"))) {
		(void) fprintf(stderr,
		  "Error during edit; transaction not entered.\n");
		goto kaboom;
	}

	fd = open(temp_file,O_RDONLY,0);
	if (fd < 0) {
		(void) fprintf(stderr,"No file; not entered.\n");
		goto kaboom;
	}
	tf = unix_tfile(fd);

	dsc_add_trn(&nbsrc, tf, "Reason for this meeting", 0, &txn_no,
		    &result);
	if (result) {
		fprintf(stderr, "mkds: Error adding transaction: %s",
			error_message(result));
		goto kaboom;
	}
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		txn_no,long_name);

	(void) close(fd);

	printf("\n");
	if (getyn("Would you like to announce this meeting [y]? ",'Y')) {
		printf("\n");
		for (;;) {
			printf("Announce in what meeting? ");
			if (!gets(ann_mtg))
				exit(1);
			dsc_get_mtg(nbsrc.user_id,strtrim(ann_mtg),
				    &nbdest,&result);
			if (!result)
				break;
			fprintf(stderr, "Meeting not found in search path.\n");
		}

		fd = open(temp_file,O_RDONLY,0);
		if (fd < 0) {
			(void) fprintf(stderr,"Temporary file disappeared!\n");
			goto kaboom;
		}

		tf = unix_tfile(fd);

		dsc_announce_mtg(&nbsrc, &nbdest, public, tf,
				 &txn_no, &result);

		if (result) {
			(void) fprintf(stderr,
				       "mkds: Error adding transaction: %s\n",
				       error_message(result));
			(void) fprintf(stderr,
			   "Use the announce_meeting (anm) request in discuss.\n");
		}
		else (void) printf("Transaction [%04d] entered in the %s meeting.\n",
				   txn_no, nbdest.aliases[0]);

		(void) close(fd);
	}
		
	error = 0;

kaboom:

	(void) unlink(temp_file);

	if (error && delmtg) {
		fprintf(stderr,"\nError encountered - deleting meeting.\n");
		remove_mtg(mtg_path,&result);
		if (result)
		     perror("Can't delete meeting");
	}
	term_rpc();
	exit(!error);
}

getyn(prompt,def)
char *prompt,def;
{
	char yn_inp[128];

	for (;;) {
		(void) printf("%s ",prompt);
		if (!gets(yn_inp))
		        exit(1);
		if (yn_inp[0] == '\0')
			yn_inp[0] = def;
		if (cupper(yn_inp[0]) == 'Y' || cupper(yn_inp[0]) == 'N')
			return (cupper(yn_inp[0]) == 'Y');
		printf("Please enter 'Yes' or 'No'\n\n");
	}
}
#include <ctype.h>

char *strtrim(cp)
	register char *cp;
{
	register int c;
	register char *cp1;

	while ((c = *cp) && isspace (c)) cp++;
	cp1 = cp;
	while (*cp1) cp1++;
	do {
		cp1--;
	} while (cp1 > cp && isspace (*cp1));
	cp1++;
	*cp1 = '\0';
	return cp;
}
