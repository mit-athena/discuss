/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v 1.10 1987-04-09 00:11:39 rfrench Exp $
 *	$Locker:  $
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.9  87/04/08  03:53:25  wesommer
 * Added del_mtg
 * 
 * Revision 1.8  87/04/06  16:00:56  spook
 * More error-message printing.
 * 
 * Revision 1.7  87/03/22  05:21:50  spook
 * Rewritten for new interfaces and new format.
 * 
 * Revision 1.6  87/02/04  16:10:26  srz
 * Changed fcntl.h -> file.h
 * 
 * Revision 1.5  86/12/07  16:04:09  rfrench
 * Globalized sci_idx
 * 
 * Revision 1.4  86/12/07  00:38:50  rfrench
 * Killed ../include
 * 
 * Revision 1.3  86/12/07  00:20:45  rfrench
 * Made adding a meeting louder and more accurate
 * 
 * Revision 1.2  86/12/03  15:06:02  rfrench
 * Fixed bug in add_mtg when specifying both tran #s and meeting names
 * 
 * Revision 1.1  86/11/24  20:18:55  rfrench
 * Initial revision
 * 
 *
 */

#ifndef lint
static char *rcsid_addmtg_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v 1.10 1987-04-09 00:11:39 rfrench Exp $";
#endif lint

#include <strings.h>
#include <stdio.h>
#include <sys/file.h>
#include "tfile.h"
#include "types.h"
#include "interface.h"
#include "globals.h"

extern char *malloc();
extern tfile unix_tfile();
int parse_add_trn();

name_blk *set;
int num;

add_mtg(argc, argv)
     int argc;
     char **argv;
{
	int i, *used;
	name_blk nb;
	int code,have_names;
	char *user,*realm,long_name[80],cerror[80];
	char auser_id[BUFSIZ];
	trn_info t_info;
	selection_list *trn_list,*trn_temp;

	used = (int *)calloc(argc, sizeof(int));
	user = "";
	realm = user;

	dsc_expand_mtg_set(user_id, "*", &set, &num, &code);

	i = 1;
	have_names = 0;
	if (dsc_public.attending) {
		dsc_get_mtg_info(&dsc_public.nb,
				 &dsc_public.m_info,&code);
		if (code) {
			(void) ss_perror(sci_idx, code,
					 "Can't get meeting info");
			return;
		}
		dsc_get_trn_info(&dsc_public.nb,
				 dsc_public.current, &t_info, &code);
		if (code)
			t_info.current = 0;
		else
			free(t_info.subject);
		t_info.subject = NULL;
		free(t_info.author);
		t_info.author = NULL;
		if (argc == 1) {
			trn_list = trn_select(&t_info, "current",
					      (selection_list *)NULL, &code);
			if (code) {
				ss_perror(sci_idx, code, "");
				free((char *)trn_list);
				return;
			}
			used[1] = 1;
		}
		else {
			trn_list = (selection_list *)NULL;
			for (;i<argc;i++) {
				trn_temp = trn_select(&t_info, argv[i],
						      trn_list, &code);
				if (code)
					break;
				trn_list = trn_temp;
				used[i] = 1;
			}
		}
		(void) sl_map(parse_add_trn,trn_list);
		have_names = 1;
	}
	     
	for (;i<argc;i++) {
		if (!strcmp("-user", argv[i])) {
			if (i == argc-1) {
				fprintf(stderr,
					"Missing argument for -user\n");
				goto punt;
			}
			if (user[0] != '\0') {
				fprintf(stderr,
					"Only one of -user, -public allowed\n");
				goto punt;
			}
			used[i] = 1;
			user = argv[++i];
			used[i] = 1;
		}
		else if (!strcmp(argv[i],"-public")) {
			if (user[0] != '\0') {
				fprintf(stderr,
					"Only one of -user, -public allowed\n");
				goto punt;
			}
			user = "discuss";
			used[i] = 1;
		}
		else if (*argv[i] == '-') {
			fprintf(stderr,
				"Unknown control argument %s\n",
				argv[i]);
			free((char *)used);
			return;
		}
		else {
			have_names = 1;
		}
	}

	strcpy(auser_id, user_id);
	if (user[0] != '\0')
		strcpy(auser_id, user);
	{
		register char *at = index(auser_id, '@');
		if (at) *at = '\0';
	}
	strcat(auser_id, "@");
	if (realm[0] != '\0')
		strcat(auser_id, realm);
	else
		strcat(auser_id, index(user_id, '@')+1);

	if (!have_names) {
		(void) fprintf(stderr,
			       "Usage:  %s {-user user|-public} [mtg_names]\n",
			       argv[0]);
		(void) fprintf(stderr,
			       "        %s transactions\n", argv[0]);
		goto punt;
	}

	for (i = 1; i < argc; i++) {
		if (!used[i]) {
			dsc_get_mtg (auser_id, argv[i], &nb, &code);
			if (code) {
				sprintf(cerror, "Getting meeting name (%s)",
					argv[i]);
				ss_perror(sci_idx, code, cerror);
			}
			else
				add_the_mtg("",&nb,0,&code);
		}
	}

	free((char *)set);

 punt:
	free((char *)used);
	return;
}

parse_add_trn(trn_no)
int trn_no;
{
	int code,fd,dummy;
	char tempbfr[256],host[100],*short_name;
	char cerror[80];
	tfile tf;
	FILE *fp;
	name_blk nb;
	mtg_info m_info;

	unlink(temp_file);
	fd = open(temp_file,O_WRONLY|O_CREAT,0700);
	tf = unix_tfile(fd);
	dsc_get_trn(&dsc_public.nb,trn_no,tf,&code);
	close(fd);
	tclose(tf,&dummy);
	tdestroy(tf);

	if (code) {
		sprintf(cerror, "Can't read transaction [%04d]", trn_no);
		goto lose;
	}
	
	fp = fopen(temp_file,"r");
	if (!fp)
		goto not_ann;
	if (!fgets(tempbfr,256,fp))
		goto not_ann;
	if (strncmp(tempbfr,"  Meeting Name:  ",17))
		goto not_ann;
	if (!fgets(tempbfr,256,fp))
		goto not_ann;
	if (strncmp(tempbfr,"  Host:          ",17))
		goto not_ann;
	tempbfr[strlen(tempbfr)-1] = '\0';
	strcpy(host,tempbfr+17);
	if (!fgets(tempbfr,256,fp))
		goto not_ann;
	if (strncmp(tempbfr,"  Pathname:      ",17))
		goto not_ann;
	fclose(fp);
	tempbfr[strlen(tempbfr)-1] = '\0';
	nb.date_attended = nb.last = 0;
	nb.hostname = malloc((unsigned)strlen(host)+1);
	strcpy(nb.hostname,host);
	nb.pathname = malloc((unsigned)strlen(tempbfr)-16);
	strcpy(nb.pathname, tempbfr+17);
	nb.user_id = malloc((unsigned)strlen(user_id)+1);
	strcpy(nb.user_id, user_id);
	nb.aliases = (char **)NULL;
	dsc_get_mtg_info(&nb, &m_info, &code);
	if (code) {
		sprintf(cerror,
			"Can't get meeting info for transaction [%04d]",
			trn_no);
		goto lose;
	}

	short_name = rindex(tempbfr,'/');
	if (!short_name)
		short_name = rindex(tempbfr,':');
	nb.aliases = (char **)calloc(3, sizeof(char *));
	nb.aliases[0] = malloc(strlen(m_info.long_name)+1);
	strcpy(nb.aliases[0], m_info.long_name);
	nb.aliases[1] = malloc(strlen(short_name));
	strcpy(nb.aliases[1],short_name+1);
	nb.aliases[2] = (char *)NULL;
	(void) add_the_mtg(host,&nb,trn_no,&code);
	return(0);		/* add_the_mtg prints error messages */

 not_ann:
	code = 0;
	sprintf(cerror,
		"Transaction [%04d] is not a properly formatted meeting announcement",
		trn_no);
 lose:
	ss_perror(sci_idx, code, cerror);
	return (0);
}

add_the_mtg(host,nb,tran,code)
	char *host;
	name_blk *nb;
	int tran,*code;
{
	struct _dsc_pub dsc_temp;
	char cerror[100];
	name_blk *nbp;
	int j;

	dsc_get_mtg_info(nb, &dsc_temp.m_info,code);
	if (*code) {
		if (tran)
			sprintf(cerror,"Transaction [%04d] ",tran);
		else
			*cerror = '\0';
		sprintf(cerror+strlen(cerror), "Meeting name (%s)",
			nb->aliases[0]);
		ss_perror(sci_idx,*code,cerror);
		return (*code);
	}
	nb->date_attended = 0;
	nb->last = 0;
	/* see if we're already attending... */
	for (j = 0,nbp = set; j < num; j++,nbp++) {
		if (!strcmp (nbp ->hostname, nb->hostname) && !strcmp(nbp->pathname, nb->pathname)) {
			nb->date_attended = nbp -> date_attended;
			nb->last = nbp -> last;
			if (!strcmp (nbp->aliases[0], nb->aliases[0])) {
				if (tran)
					sprintf(cerror, "Transaction [%04d] ",
						tran);
				else
					*cerror = '\0';
				sprintf(cerror+strlen(cerror),
					"Meeting %s (%s) is duplicated",
					nb->aliases[0],nb->aliases[1]);
				ss_perror(sci_idx,0,cerror);
				return (0);
			}
		}
	}
	dsc_update_mtg_set(user_id, nb, 1, code);
	if (*code) {
		sprintf(cerror,"Setting meeting name (%s)",nb->aliases[0]);
		ss_perror(sci_idx, *code, cerror);
	}
	else {
		if (tran)
			printf("Transaction [%04d] ",tran);
		printf("Meeting %s (%s) added\n",nb->aliases[0],nb->aliases[1]);
	}
	return (0);
}

del_mtg(argc, argv)
     int argc;
     char **argv;
{
	int i, *used;
	name_blk nb;
	int code,have_names;
	char *user,*realm,cerror[80];
	char auser_id[BUFSIZ];

	used = (int *)calloc(argc, sizeof(int));
	user = "";
	realm = user;

	have_names = 0;
	     
	for (i=1; i<argc; i++) {
		if (*argv[i] == '-') {
			fprintf(stderr,
				"Unknown control argument %s\n",
				argv[i]);
			free((char *)used);
			return;
		}
		else {
			have_names = 1;
		}
	}

	strcpy(auser_id, user_id);
	if (user[0] != '\0')
		strcpy(auser_id, user);
	{
		register char *at = index(auser_id, '@');
		if (at) *at = '\0';
	}
	strcat(auser_id, "@");
	if (realm[0] != '\0')
		strcat(auser_id, realm);
	else
		strcat(auser_id, index(user_id, '@')+1);

	if (!have_names) {
		(void) fprintf(stderr,
			       "Usage:  %s [mtg_names]\n",
			       argv[0]);
		goto punt;
	}

	for (i = 1; i < argc; i++) {
		if (!used[i]) {
			dsc_get_mtg (auser_id, argv[i], &nb, &code);
			if (code) {
				sprintf(cerror, 
					"while getting meeting name (%s)",
					argv[i]);
				ss_perror(sci_idx, code, cerror);
			}
			nb.status |= DSC_ST_DELETED;
			dsc_update_mtg_set(auser_id, &nb, 1, &code);
			if (code) {
				sprintf(cerror, "while deleting meeting %s\n",
					argv[i]);
				ss_perror(sci_idx, code, cerror);
			}
		}
	}

 punt:
	free((char *)used);
	return;
}

