/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v 1.4 1986-12-07 00:38:50 rfrench Exp $
 *	$Locker:  $
 *
 *	$Log: not supported by cvs2svn $
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
static char *rcsid_addmtg_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v 1.4 1986-12-07 00:38:50 rfrench Exp $";
#endif lint

#include <strings.h>
#include <stdio.h>
#include <fcntl.h>
#include "tfile.h"
#include "types.h"
#include "interface.h"
#include "globals.h"

extern char *malloc();
extern tfile unix_tfile();
int parse_add_trn();

name_blk *set;
int num;
int sci_kludge; /* Temporary sci_idx - begging to be fixed */

add_mtg(sci_idx, argc, argv)
     int sci_idx, argc;
     char **argv;
{
	int i, *used;
	name_blk nb;
	int code,have_names;
	char *user,*realm,long_name[80],cerror[80];
	trn_info t_info;
	selection_list *trn_list,*trn_temp;

	sci_kludge = sci_idx;
	used = (int *)malloc(sizeof(int)*argc);
	bzero(used, sizeof(int)*argc); /* bletch */
	user = "";
	realm = user;

	expand_mtg_set("","","*",&set,&num);

	i = 1;
	have_names = 0;
	if (dsc_public.attending) {
		dsc_get_mtg_info(dsc_public.mtg_uid,&dsc_public.m_info,&code);
		if (code) {
			(void) ss_perror(sci_idx,code,"Can't get meeting info");
			return;
		}
		dsc_get_trn_info(dsc_public.mtg_uid,dsc_public.current,&t_info,
				 &code);
		if (code)
			t_info.current = 0;
		else
			free(t_info.subject);
		t_info.subject = NULL;
		free(t_info.author);
		t_info.author = NULL;
		if (argc == 1) {
			trn_list = trn_select(&t_info,"current",
					      (selection_list *)NULL,&code);
			if (code) {
				ss_perror(sci_idx,code,"");
				free(trn_list);
				return;
			}
			used[1] = 1;
		}
		else {
			trn_list = (selection_list *)NULL;
			for (;i<argc;i++) {
				trn_temp = trn_select(&t_info,argv[i],
						      trn_list,&code);
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
			free(used);
			return;
		}
		else {
			have_names = 1;
		}
	}

	if (user[0] == '\0')
		user = "discuss";

	if (!have_names) {
		(void) fprintf(stderr,
			       "Usage:  %s {-user user} {-public} [mtg_names] [transactions]\n",
			       argv[0]);
		(void) fprintf(stderr,
			       "        %s transactions\n", argv[0]);
		goto punt;
	}

	for (i = 1; i < argc; i++) {
		if (!used[i]) {
			get_mtg_unique_id (realm, user, argv[i], &nb, &code);
			if (code) {
				sprintf(cerror,"Getting meeting name (%s)",argv[i]);
				ss_perror(sci_idx, code, cerror);
			}
			else
				add_the_mtg("",&nb,long_name,0,&code);
		}
punt_this_one: ;
	}

	free(set);

 punt:
	free(used);
	return;
}

parse_add_trn(trn_no)
int trn_no;
{
	int code,fd,dummy;
	char tempbfr[256],host[50],long_name[80],*realm,*user,*short_name;
	char cerror[80];
	tfile tf;
	FILE *fp;
	name_blk nb;

	unlink(temp_file);
	fd = open(temp_file,O_WRONLY|O_CREAT,0700);
	tf = unix_tfile(fd);
	dsc_get_trn(dsc_public.mtg_uid,trn_no,tf,&code);

	close(fd);
	tclose(tf,&dummy);
	tdestroy(tf);

	if (code)
		return;
	
	fp = fopen(temp_file,"r");
	if (!fp)
		goto not_ann;
	if (!fgets(tempbfr,256,fp))
		goto not_ann;
	if (strncmp(tempbfr,"  Meeting Name:  ",17))
		goto not_ann;
	if (!fgets(tempbfr,256,fp))
		goto not_ann;
	if (strncmp(tempbfr,"  ID:            ",17))
		goto not_ann;
	fclose(fp);
	tempbfr[strlen(tempbfr)-1] = '\0';
	nb.date_attended = nb.last = 0;
	short_name = rindex(tempbfr,'/');
	if (!short_name)
		short_name = rindex(tempbfr,':');
	strcpy(nb.mtg_name,short_name+1);
	strncpy(host,tempbfr+17,index(tempbfr+17,':')-tempbfr-17);
	strcpy(nb.unique_id,tempbfr+17);
	strcpy(nb.user,"");
	add_the_mtg(host,&nb,long_name,trn_no,&code);
	return (0); /* Ignore errors! */

 not_ann:

	sprintf(cerror,"Transaction [%04d] is not a meeting announcement",
		trn_no);
	ss_perror(sci_kludge,0,cerror);
	return (0);
}

add_the_mtg(host,nb,long_name,tran,code)
	char *host;
	name_blk *nb;
	char *long_name;
	int tran,*code;
{
	struct _dsc_pub dsc_temp;
	char realm[100],cerror[100];
	name_blk *nbp;
	int j;

	dsc_get_mtg_info(nb->unique_id,&dsc_temp.m_info,code);
	if (*code) {
		if (tran)
			sprintf(cerror,"Transaction [%04d] ",tran);
		else
			*cerror = '\0';
		sprintf(cerror+strlen(cerror),"Meeting name (%s)",nb->mtg_name);
		ss_perror(sci_kludge,*code,cerror);
		return (*code);
	}
	strcpy(long_name,dsc_temp.m_info.long_name);
	nb->date_attended = 0;
	nb->last = 0;
	/* see if we're already attending... */
	for (j = 0,nbp = set; j < num; j++,nbp++) {
		if (!strcmp (nbp -> unique_id, nb->unique_id)) {
			nb->date_attended = nbp -> date_attended;
			nb->last = nbp -> last;
			if (!strcmp (nbp -> mtg_name, nb->mtg_name)) {
				if (tran)
					sprintf(cerror,"Transaction [%04d] ",tran);
				else
					*cerror = '\0';
				sprintf(cerror+strlen(cerror),"Meeting %s (%s) is duplicated",
					long_name,nb->mtg_name);
				ss_perror(sci_kludge,0,cerror);
				return;
			}
		}
	}
	update_mtg_set(realm, "", nb, 1, code);
	if (*code) {
		sprintf(cerror,"Setting meeting name (%s)",nb->mtg_name);
		ss_perror(sci_kludge, *code, cerror);
	}
	else {
		if (tran)
			printf("Transaction [%04d] ",tran);
		printf("Meeting %s (%s) added\n",long_name,nb->mtg_name);
	}
}
