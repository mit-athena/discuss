/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *  addmtg () --
 *  	The add_meeting command in discuss.  This request adds a
 *	meeting to the user's list of meetings.  There are several
 *	ways that this can occur -- the user could put the meeting
 *	on the command line, in the form of host:pathname.
 *	
 *	Or the user could put some transaction specifiers on the
 *	command line, in which case they are used as meeting announcements.
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v 1.29 1994-03-25 16:32:25 miki Exp $
 *	$Locker:  $
 *
 */

#ifndef lint
static char rcsid_addmtg_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/addmtg.c,v 1.29 1994-03-25 16:32:25 miki Exp $";
#endif lint

#include <string.h>
#include <stdio.h>
#include <netdb.h>
#include <sys/file.h>
#ifdef SOLARIS
#include <fcntl.h>
#endif
#include <discuss/discuss.h>
#include "globals.h"
#include "ss.h"

extern char *malloc(), *calloc();
extern tfile unix_tfile();
int add_mtg_it();

static int mtgs_added;
static del_the_mtg();

name_blk *set;
int num;

add_mtg(argc, argv)
     int argc;
     char **argv;
{
	int i, *used;
	name_blk nb;
	int code,have_names;
	char *user,*realm,cerror[80],*pathp,*hostp;
	char auser_id[BUFSIZ];
	trn_info t_info;
	selection_list *trn_list,*trn_temp;

	used = (int *)calloc(argc+1, sizeof(int));
	user = "";
	realm = user;
	set = NULL;
	dsc_expand_mtg_set(user_id, "*", &set, &num, &code);

	i = 1;
	have_names = 0;

	/* If we're attending a meeting, try to interpret the arguments
	   as transaction specifiers for the current meeting.  If no
	   arguments are given, then we default to the current transaction */
	if (dsc_public.attending) {
		dsc_destroy_mtg_info(&dsc_public.m_info);
		dsc_get_mtg_info(&dsc_public.nb,
				 &dsc_public.m_info,&code);	/* get mtg info for trn mapping */
		if (code) {
			(void) ss_perror(sci_idx, code,
					 "Can't get meeting info for current meeting");
			goto punt;
		}
		dsc_get_trn_info(&dsc_public.nb,
				 dsc_public.current, &t_info, &code);
		if (code)
			t_info.current = 0;
		dsc_destroy_trn_info (&t_info);
		if (argc == 1) {
			trn_list = trn_select(&t_info, "current",
					      (selection_list *)NULL, &code);
			if (code) {
				ss_perror(sci_idx, code, "");
				sl_free(trn_list);
				goto punt;
			}
			used[1] = 1;
		} else {
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
		mtgs_added = FALSE;
		(void) sl_map(add_mtg_it,trn_list,FALSE);
		sl_free(trn_list);
		if (!mtgs_added)		/* Check if we had args */
		     for (i = 1; i <argc; i++)
			  if (used[i]) {
			       ss_perror(sci_idx, 0, "No transactions selected");
			       break;
			  }
		have_names = 1;
	}


	/* now we parse the command line for '-user' and meeting names */
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
			goto punt;
		}
		else {
			have_names = 1;
		}
	}

	/* Convert user given to include realm name */
	strcpy(auser_id, user_id);
	if (user[0] != '\0')
		strcpy(auser_id, user);
	{
		register char *at = strchr(auser_id, '@');
		if (at) *at = '\0';
	}
	strcat(auser_id, "@");
	if (realm[0] != '\0')
		strcat(auser_id, realm);
	else
		strcat(auser_id, strchr(user_id, '@')+1);

	if (!have_names) {
		(void) fprintf(stderr,
			       "Usage:  %s {-user user|-public} [mtg_names]\n",
			       argv[0]);
		(void) fprintf(stderr,
			       "        %s transactions\n", argv[0]);
		goto punt;
	}

	/* add the meeting names we're given */
	for (i = 1; i < argc; i++) {
	     if (!used[i]) {
		  if ((pathp = strchr (argv[i], ':')) != NULL) {
		       hostp = argv[i];
		       *pathp++ = '\0';	
		       sprintf(cerror, "Adding meeting %s:%s", hostp,pathp);
		       hostpath_to_nb (hostp, pathp, &nb, &code);
		       if (code) {
			    ss_perror(sci_idx, code, cerror);
			    continue;
		       }
		  } else {
		       sprintf(cerror, "Adding meeting (%s)", argv[i]);
		       dsc_get_mtg (auser_id, argv[i], &nb, &code);
		       if (code) {
			    ss_perror(sci_idx, code, cerror);
			    continue;
		       }
		  }
		  add_the_mtg(&nb,&code);
		  if (code && code != DISC_ACTION_NOT_PERFORMED)
		       ss_perror(sci_idx, code, cerror);
		 else if (code != DISC_ACTION_NOT_PERFORMED)
		      printf ("Meeting %s (%s) added.\n", nb.aliases[0], nb.aliases[1]);
		  dsc_destroy_name_blk(&nb);
	     }
	}

punt:
	dsc_destroy_mtg_set (set, num);
	free((char *)used);
	return;
}

static
add_mtg_it(t_infop, codep)
trn_info3 *t_infop;
int *codep;
{
     char cerror[80];
     name_blk nb;

     mtgs_added = TRUE;
     if (*codep != 0) {
	  sprintf (cerror, "Transaction [%04d]", t_infop->current);
	  ss_perror(sci_idx, *codep, cerror);
	  *codep = 0;
	  return;
     }

     parse_mtg_info (&dsc_public.nb, t_infop->current, &nb, codep);
     if (*codep != 0) {
	  if (*codep == DISC_NOT_ANNOUNCEMENT) {
	       sprintf (cerror, "Transaction [%04d] is not a properly formatted meeting announcement", t_infop->current);
	       ss_perror (sci_idx, 0, cerror);
	  } else {
	       sprintf (cerror, "Transaction [%04d]", t_infop->current);
	       ss_perror(sci_idx, *codep, cerror);
	  }
	  return;
     }
     
     add_the_mtg (&nb, codep);
     if (*codep != 0 && *codep != DISC_ACTION_NOT_PERFORMED) {
	  sprintf (cerror, "Error adding meeting in transaction [%04d]", t_infop->current);
	  ss_perror(sci_idx, *codep, cerror);
     } else if (*codep != DISC_ACTION_NOT_PERFORMED)
	  printf ("Transaction [%04d] Meeting %s (%s) added.\n", t_infop->current, nb.aliases[0], nb.aliases[1]);
     
     dsc_destroy_name_blk (&nb);
     *codep = 0;
     return;
}

parse_mtg_info(mtg_nbp, trn_no, result_nbp, code)
name_blk *mtg_nbp;
int trn_no;
name_blk *result_nbp;
int *code;
{
     int fd,dummy;
     char tempbfr[256],host[100];
     tfile tf;
     FILE *fp;

     *code = 0;

     /* initialize puntable state */
     fp = NULL;

     unlink(temp_file);
     fd = open(temp_file,O_WRONLY|O_CREAT,0700);
     tf = unix_tfile(fd);
     dsc_get_trn(mtg_nbp,trn_no,tf,code);
     close(fd);
     tclose(tf,&dummy);
     tdestroy(tf);

     if (*code)
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
     if (strncmp(tempbfr,"  Host:          ",17))
	  goto not_ann;
     tempbfr[strlen(tempbfr)-1] = '\0';
     strcpy(host,tempbfr+17);
     if (!fgets(tempbfr,256,fp))
	  goto not_ann;
     if (strncmp(tempbfr,"  Pathname:      ",17))
	  goto not_ann;
     fclose(fp);
     fp = NULL;
     tempbfr[strlen(tempbfr)-1] = '\0';

     hostpath_to_nb (host, &tempbfr[17], result_nbp, code);
     goto punt;

not_ann:
     *code = DISC_NOT_ANNOUNCEMENT;

punt:
     if (fp != NULL)
	  fclose(fp);

     return;
}

hostpath_to_nb (host, path, nbp, code)
char *host, *path;
name_blk *nbp;
int *code;
{
     struct hostent *hp;
     mtg_info m_info;
     char *short_name;

     nbp -> hostname = nbp -> pathname = nbp -> user_id = nbp -> spare = NULL;
     nbp -> date_attended = nbp -> last = nbp -> status = 0;

     hp = gethostbyname (host);
     if (hp != NULL)
	  host = hp -> h_name;			/* use canonical if possible */
     nbp -> hostname = malloc((unsigned)strlen(host)+1);
     strcpy(nbp -> hostname,host);
     nbp -> pathname = malloc((unsigned)strlen(path)+1);
     strcpy(nbp -> pathname, path);
     nbp -> user_id = malloc((unsigned)strlen(user_id)+1);
     strcpy(nbp -> user_id, user_id);
     nbp -> aliases = (char **)NULL;
     dsc_get_mtg_info(nbp, &m_info, code);
     if (*code) {
	  if (*code == NO_ACCESS) {
	       *code = CANT_ATTEND;		/* friendlier error msg */
	  }
	  goto punt;
     }
     short_name = strrchr(path,'/');
     if (!short_name)
	  short_name = strrchr(path,':');
     nbp -> aliases = (char **)calloc(3, sizeof(char *));
     nbp -> aliases[0] = malloc(strlen(m_info.long_name)+1);
     strcpy(nbp -> aliases[0], m_info.long_name);
     nbp -> aliases[1] = malloc(strlen(short_name));
     strcpy(nbp -> aliases[1],short_name+1);
     nbp -> aliases[2] = (char *)NULL;
     *(nbp->spare = malloc(1)) = '\0';
     free(m_info.location);
     free(m_info.chairman);
     free(m_info.long_name);

     return;

punt:
     dsc_destroy_name_blk (nbp);
     return;
}

add_the_mtg(new_nbp,code)
	name_blk *new_nbp;
	int *code;
{
	name_blk *nbp,temp_nb;
	char question[100];
	int j;

	/* see if we're already attending... */
	for (j = 0,nbp = set; j < num; j++,nbp++) {
		if (!namcmp (nbp ->hostname, new_nbp->hostname) && !strcmp(nbp->pathname, new_nbp->pathname)) {
			*code = DISC_DUPLICATE_MTG;
			return;
		}
	}

	for (j = 0; new_nbp->aliases[j] != NULL; j++) {
	     dsc_get_mtg (user_id, new_nbp->aliases[j], &temp_nb, code);
	     if (*code == 0) {				/* already exists */
		  sprintf (question, "Meeting %s already exists.\nDo you wish to delete the old one and add the new one? ",new_nbp -> aliases[j]);
		  if (!command_query (question)) {
		       printf ("Meeting not added.\n");
		       dsc_destroy_name_blk (&temp_nb);
		       *code = DISC_ACTION_NOT_PERFORMED;
		       return;
		  }

		  del_the_mtg (&temp_nb, code);
		  if (*code != 0) {
		       dsc_destroy_name_blk (&temp_nb);
		       return;
		  }
		  dsc_destroy_name_blk(&temp_nb);
	     }
	}
	     
	dsc_update_mtg_set(user_id, new_nbp, 1, code);
	if (*code) {
		return;
	}
	return;
}

del_mtg(argc, argv)
     int argc;
     char **argv;
{
	int i, *used;
	name_blk nb;
	int code,have_names;
	char *user,*realm;
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
		register char *at = strchr(auser_id, '@');
		if (at) *at = '\0';
	}
	strcat(auser_id, "@");
	if (realm[0] != '\0')
		strcat(auser_id, realm);
	else
		strcat(auser_id, strchr(user_id, '@')+1);

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
				ss_perror(sci_idx, code, argv[i]);
				continue;
			}
			del_the_mtg (&nb, &code);
			if (code) {
				ss_perror(sci_idx, code, argv[i]);
			}
		}
	}

 punt:
	free((char *)used);
	return;
}

static
del_the_mtg (nbp,code)
name_blk *nbp;
int *code;
{
     /* If we're attending the meeting we're deleting,
	we leave it, so than we don't accidentally
	add it again when we leave it */
     if (dsc_public.attending && !strcmp (nbp->aliases[0], dsc_public.nb.aliases[0]))
	  leave_mtg();
     nbp->status |= DSC_ST_DELETED;
     dsc_update_mtg_set(user_id, nbp, 1, code);
}
     
/*
 *
 *  command_query () --
 *	Routine to ask the user a question, and return TRUE/FALSE depending
 *	on whether it is 'yes'/'no'
 *
 */
command_query (question)
char *question;
{
     int result;
     char answer [20];

     printf ("%s", question);
     fflush (stdout);
     if (fgets (answer, 10, stdin) == NULL)
	  return(FALSE);


     return (answer [0] == 'Y' || answer [0] == 'y');
}
