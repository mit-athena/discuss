/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/mkds.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/mkds.c,v 1.2 1986-11-24 20:07:21 rfrench Exp $
 *	$Locker:  $
 *
 *	$Log: not supported by cvs2svn $
 */

#ifndef lint
static char *rcsid_mkds_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/mkds.c,v 1.2 1986-11-24 20:07:21 rfrench Exp $";
#endif lint

#include "../include/tfile.h"
#include "../include/dsc_et.h"
#include "../include/config.h"
#include "../include/interface.h"
#include "../include/rpc.h"
#include "../client/globals.h"
#include <sys/time.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <netdb.h>

struct _dsc_pub dsc_public;

#define cupper(x) (islower(x)?toupper(x):(x))

main(argc,argv)
int argc;
char *argv[];
{
	extern tfile unix_tfile();
	name_blk nb;
	char long_name[100],short_name[100],host[50],mtg_path[100];
	char temp_file[64],temp_file2[64],tempbfr[256];
	char ann_mtg[100],subject[100];
	char *whoami;
	int public = 0,error = 1,result,remove=0,delmtg=0;
	int fd,txn_no;
	tfile tf;
	FILE *fp,*fp2;

	init_dsc_err_tbl();

	(void) sprintf(temp_file,"/tmp/mtg%d.%d",getuid(),getpid());
	(void) sprintf(temp_file2,"/tmp/mtga%d.%d",getuid(),getpid());

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

	printf("Meeting directory <working dir>: ");
	(void) gets(mtg_path);
	if (!mtg_path[0])
		if (!getwd(mtg_path)) {
			(void) fprintf(stderr,"Can't get working directory\n");
			goto kaboom;
		}
	if (!remove) {
		printf("\nLong meeting name: ");
		(void) gets(long_name);
	}
	printf("\nShort meeting name: ");
	(void) gets(short_name);
	(void) strcat(mtg_path,"/");
	(void) strcat(mtg_path,short_name);

	make_unique(mtg_path,nb.unique_id,host,nb.realm,&result);
	if (result)
		goto kaboom;

	init_rpc();
	if (!open_rpc(host,"discuss",&result)) {
		(void) fprintf(stderr,"%s\n",error_message(result));
		exit(1);
	}

	if (result)
		(void) fprintf(stderr,"Warning: %s\n",error_message(result));

	if (remove) {
		remove_mtg(mtg_path,&result);
		if (result)
			(void) fprintf(stderr,"Can't remove meeting: %s\n",
				       error_message(result));
		error = result;
		goto kaboom;
	}

	printf("\n");
	public = getyn("Should this meeting be public <N>? ",'N');

	create_mtg(mtg_path,long_name,public,&result);
	if (result) {
		fprintf(stderr,"%s\n",error_message(result));
		goto kaboom;
	}

	delmtg = 1;

	nb.date_attended = 0;
	nb.last = 0;
	(void) strcpy(nb.mtg_name,short_name);
	(void) strcpy(nb.user,"");
	update_mtg_set(host,"",&nb,1,&result);
	if (result) {
		fprintf(stderr,"Error setting meeting name\n");
		goto kaboom;
	}

	printf("\nYou must now enter the initial transaction.\n");
	printf(
	  "This transaction will serve as an introduction to the meeting.\n");

	(void) unlink(temp_file);
	(void) unlink(temp_file2);

	if (edit(temp_file)) {
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

	dsc_add_trn(nb.unique_id,tf,"Reason for this meeting",0,&txn_no,
		&result);
	if (result) {
		(void) fprintf(stderr,"Error adding transation: %s\n",
			error_message(result));
		goto kaboom;
	}
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		txn_no,long_name);

	(void) close(fd);

	printf("\n");
	if (!getyn("Would you like to announce this meeting <Y>? ",'Y')) {
		error = 0;
		goto kaboom;
	}
	fp = fopen(temp_file2,"w");
	if (!fp) {
		fprintf(stderr,"Can't open temporary file\n");
		goto kaboom;
	}
	fprintf(fp,"  Meeting Name:  %s\n",long_name);
	fprintf(fp,"  ID:            %s\n",nb.unique_id);
	fprintf(fp,"  Participation: %s\n",public?"Public":"Private");
	fprintf(fp,"\n");
	fp2 = fopen(temp_file,"r");
	while (fgets(tempbfr,256,fp2))
		fprintf(fp,"%s",tempbfr);
	(void) fclose(fp);
	(void) fclose(fp2);

	for (;;) {
		printf("\nAnnounce in what meeting? ");
		(void) gets(ann_mtg);
		get_mtg_unique_id("","",ann_mtg,&nb,&result);
		if (!result)
			break;
		printf("Meeting not found in search path.\n");
	}

	fd = open(temp_file2,O_RDONLY,0);
	if (fd < 0) {
		(void) fprintf(stderr,"Can't open temporary file.\n");
		goto kaboom;
	}
	tf = unix_tfile(fd);

	(void) sprintf(subject,"%s meeting",long_name);
	dsc_add_trn(nb.unique_id,tf,subject,0,&txn_no,&result);
	if (result) {
		(void) fprintf(stderr,"Error adding transation: %s\n",
			error_message(result));
		goto kaboom;
	}
	(void) printf("Transaction [%04d] entered in the %s meeting.\n",
		txn_no,nb.mtg_name);

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

/*
 *
 * resolve_mtg:	Procedure to resolve a user meeting into its host and
 *							 pathname.
 *
 */

make_unique(path,unique,host,realm,result)
char *path,*unique,*host,*realm;
int *result;
{
	char *colon,bitbucket[128];
	int host_len;
	struct hostent *hp;
	struct timeval *tp;
	struct timezone *tzp;

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
	ExpandHost(hp,bitbucket,realm);
	(void) strcpy(unique,hp->h_name);
	(void) strcat(unique,":");
	(void) gettimeofday(tp,tzp);
	(void) sprintf(unique+strlen(hp->h_name)+1,"%ld",tp->tv_sec);
	(void) strcat(unique,path);
	*result = 0;
	return;
}

/*
Subject: Meeting name
Location: blah
Participation:  public/one line description

Transaction
*/

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
