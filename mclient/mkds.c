/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/mkds.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/mkds.c,v 1.7 1987-04-08 21:40:57 rfrench Exp $
 *	$Locker:  $
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.5  87/03/22  04:51:58  spook
 * Rewritten for new interfaces.
 * 
 * Revision 1.4  87/02/12  21:40:57  spook
 * Rob's changes; removed "../include" stuff, other frobs.
 * 
 * Revision 1.3  86/12/05  20:06:12  rfrench
 * General cleanup; default directory /usr/spool/discuss
 * 
 * Revision 1.2  86/11/24  20:07:21  rfrench
 * Initial (working) revision
 * 
 */

#ifndef lint
static char *rcsid_mkds_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/mkds.c,v 1.7 1987-04-08 21:40:57 rfrench Exp $";
#endif lint

#include "tfile.h"
#include "dsc_et.h"
#include "config.h"
#include "interface.h"
#include "rpc.h"
#include "globals.h"
#include <sys/time.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <netdb.h>
#include <pwd.h>

#define cupper(x) (islower(x)?toupper(x):(x))
#define clower(x) (isupper(x)?tolower(x):(x))

char *default_dir = "/usr/spool/discuss";
char *whoami;

char *getenv(), *malloc();

main(argc,argv)
int argc;
char *argv[];
{
	extern tfile unix_tfile();
	name_blk nb;
	char long_name[100],short_name[100],module[50],mtg_path[100];
	char temp_file[64],tempbfr[256];
	char ann_mtg[100],subject[100];
	int public = 0,error = 1,result,remove=0,delmtg=0;
	int fd,txn_no,fatal_err;
	tfile tf;
	char hostname[256];
	FILE *fp,*fp2;

	init_dsc_err_tbl();

	nb.user_id = malloc(132);

	(void) sprintf(temp_file,"/tmp/mtg%d.%d",getuid(),getpid());

	whoami = rindex(argv[0],'/');
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
		register char *h;
		struct hostent *hp;
		hp = gethostbyname(hostname);
		h = (hp ? hp->h_name : hostname);
		nb.hostname = malloc(strlen(h)+1);
		strcpy(nb.hostname, h);
	}

	printf("Meeting location [default %s]: ", default_dir);
	(void) gets(mtg_path);
	if (!mtg_path[0])
		strcpy(mtg_path, default_dir);
	if (!remove) {
		printf("\nLong meeting name: ");
		(void) gets(long_name);
	}
	printf("\nYour meeting name: ");
	(void) gets(short_name);
	(void) strcat(mtg_path,"/");
	(void) strcat(mtg_path,short_name);
	nb.pathname = malloc(strlen(mtg_path)+1);
	strcpy(nb.pathname, mtg_path);

	(void) strcpy (module, getpwuid(getuid())->pw_name);
	(void) strcpy (nb.user_id, module);
	{			/* XXX - tmp kludge to get this running */
		char buf[BUFSIZ];

		init_rpc();

		strcpy(buf, "discuss@");
		strcat(buf, hostname);
		set_module (buf, &fatal_err, &result);
		if (fatal_err) {
			(void) fprintf (stderr,
					"%s: %s.  Can't set RPC module %s.\n",
					whoami,
					error_message(result),
					buf);
			exit (1);
		}
		else if (result) {
			(void) fprintf (stderr,
					"Warning: %s. Can't set RPC module.\n",
					error_message(result));
		}
	}

	if (remove) {
		remove_mtg(mtg_path,&result);
		if (result)
			(void) fprintf(stderr,"Can't remove meeting: %s\n",
				       error_message(result));
		error = result;
		goto kaboom;
	}

	printf("\n");
	public = getyn("Should this meeting be public [y]? ",'Y');

	create_mtg(mtg_path,long_name,public,&result);
	if (result) {
		fprintf(stderr, "%s.  Can't create meeting.\n",
			error_message(result));
		goto kaboom;
	}

	delmtg = 1;

	nb.date_attended = time(0);
	nb.last = 0;
	nb.status = 0;
	nb.aliases = (char **) calloc(3, sizeof(char *));
	nb.aliases[0] = malloc(strlen(long_name)+1);
	strcpy(nb.aliases[0], long_name);
	nb.aliases[1] = malloc(strlen(short_name)+1);
	strcpy(nb.aliases[1], short_name);
	nb.aliases[2] = (char *)NULL;

	dsc_update_mtg_set(module,&nb,1,&result);
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

	dsc_add_trn(&nb, tf, "Reason for this meeting", 0, &txn_no, &result);
	if (result) {
		fprintf(stderr, "mkds: Error adding transaction: %s",
			error_message(result));
		goto kaboom;
	}
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		txn_no,long_name);

	(void) close(fd);

	printf("\n");
	if (!getyn("Would you like to announce this meeting [y]? ",'Y')) {
		error = 0;
		goto kaboom;
	}

	for (;;) {
		printf("\nAnnounce in what meeting? ");
		(void) gets(ann_mtg);
		dsc_get_mtg(nb.user_id,ann_mtg,&nb,&result); /* XXX */
		if (!result)
			break;
		printf("Meeting not found in search path.\n");
	}

	fd = open(temp_file,O_RDONLY,0);
	if (fd < 0) {
		(void) fprintf(stderr,"Temporary file disappeared!\n");
		goto kaboom;
	}

	tf = unix_tfile(fd);

	dsc_announce_mtg(&nb, long_name, public, tf, &txn_no, &result);

	if (result) {
		(void) fprintf(stderr,"Error adding transation: %s\n",
			error_message(result));
		goto kaboom;
	}

	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		      txn_no, nb.aliases[0]);

	(void) close(fd);

	error = 0;

kaboom:

	(void) unlink(temp_file);

	if (error && delmtg) {
		fprintf(stderr,"\nError encountered - deleting meeting.\n");
		remove_mtg(mtg_path,&result);
	}
	term_rpc();
	exit(!error);
}

#ifdef notdef
make_unique(path,unique,host,realm,result)
char *path,*unique,*host,*realm;
int *result;
{
	char *colon,bitbucket[128],*cp;
	int host_len;
	struct hostent *hp;

	colon = index(path,':');
 
	if (!colon) {
		if (gethostname(host,50)) {
			*result = 1;
			fprintf(stderr,"Unable to get host name\n");
			return;
		}
	}
	else {
		host_len = colon-path;
		bcopy(path,host,host_len);
		host[host_len] = '\0';
		(void) strcpy(path,colon+1);
	}
	hp = gethostbyname(host);
	if (!hp) {
		*result = 1;
		fprintf(stderr,"Unable to resolve host name\n");
		return;
	}
	(void) strcpy(host,hp->h_name);
	(void) strcpy(unique,hp->h_name);
	/* Upper case unique host name */
	for (cp = unique; *cp; cp++)
	     *cp = clower(*cp);
	(void) strcat(unique,":");
	(void) sprintf(unique+strlen(hp->h_name)+1,"%d",time(0));
	(void) strcat(unique,path);
	*result = 0;
	return;
}
#endif

getyn(prompt,def)
char *prompt,def;
{
	char yn_inp[128];

	for (;;) {
		(void) printf("%s ",prompt);
		(void) gets(yn_inp);
		if (yn_inp[0] == '\0')
			yn_inp[0] = def;
		if (cupper(yn_inp[0]) == 'Y' || cupper(yn_inp[0]) == 'N')
			return (cupper(yn_inp[0]) == 'Y');
		printf("Please enter 'Yes' or 'No'\n\n");
	}
}
