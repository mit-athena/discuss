/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/ckm.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/ckm.c,v 1.23 1996-09-19 22:28:26 ghudson Exp $
 *
 */
     
#ifndef lint
static char rcsid_ckm_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/ckm.c,v 1.23 1996-09-19 22:28:26 ghudson Exp $";
#endif lint

#include <string.h>
#include <stdio.h>
#include <discuss/discuss.h>
#include "globals.h"

extern char *malloc(),*ctime(), *error_message(), *calloc();

extern int print_header;
static int display;
static int checked_meetings;

static
do_mtg(mtg_name)
     char *mtg_name;
{
     name_blk *set = NULL;
     register name_blk *nbp;
     int n_matches, i, code;
     bool updated;
     bool cur_mtg_updated = 0;
     char last_host[140], last_path[140];

     
     dsc_expand_mtg_set(user_id, mtg_name, &set, &n_matches, &code);
     if (!n_matches)
	  return (0);
     
     last_host[0] = '\0';
     last_path[0] = '\0';
     for (i = 0; i < n_matches; i++) {
	  code = 0;
	  if (interrupt)
	       break;
	  nbp = &set[i];
	  /* Test to see if we are attending this meeting */
	  if (dsc_public.attending 
	  && !strcmp(dsc_public.path, nbp->pathname)
  	  && !strcmp(dsc_public.host, nbp ->hostname)) {
	       dsc_destroy_mtg_info(&dsc_public.m_info);
	       dsc_get_mtg_info(&dsc_public.nb,
				&dsc_public.m_info, &code);
	       updated = (dsc_public.highest_seen < dsc_public.m_info.last);
	       cur_mtg_updated = updated;
	       code = 0;
	  } else {
	       dsc_updated_mtg(nbp, &updated, &code);
	       if (interrupt)
		    break;
	       if (code == NO_SUCH_TRN) {		/* Meeting lost trns */
		    updated = TRUE;
		    code = 0;
	       }
	  }
	  if (strcmp(last_path, nbp->pathname) || 
	      strcmp(last_host, nbp->hostname)) {
	       strcpy(last_host,nbp->hostname);
	       strcpy(last_path,nbp->pathname);
	       if (updated && !code)
		    nbp->status |= DSC_ST_CHANGED;
	       else {
		    nbp->status &= ~DSC_ST_CHANGED;
	       }
	       if (display && (updated || code))
	    	    do_line(nbp, code, updated);
	       if (updated)
		    print_header = 0;
	  }
     }
     if (!interrupt) {
	  dsc_update_mtg_set(user_id, set, n_matches, &code);
	  if (cur_mtg_updated)
		dsc_public.nb.status |= DSC_ST_CHANGED;
     }
     dsc_destroy_mtg_set(set, n_matches);
     return(0);
}

check_meetings (argc, argv)
     int argc;
     char **argv;
{
     int have_names = 0;
     char errbuf[BUFSIZ];
     int i, *used;
     
     used = (int *)calloc(argc, sizeof(int));
     print_header = 1;
     display = 1;
     
     for (i = 1; i < argc; i++) {
	  if (!strcmp(argv[i], "-quiet") || !strcmp(argv[i], "-q") || !strcmp(argv[i], "-no_list") || !strcmp(argv[i], "-nls")) {
		display=0; used[i]=1;
	} else if (!strcmp(argv[i], "-list") || !strcmp(argv[i], "-ls")) {
		display=1; used[i]=1;
	} else if (*argv[i] == '-') {
	       sprintf(errbuf, "Unknown control argument %s\n", argv[i]);
	       ss_perror(sci_idx, 0, errbuf);
	       free((char *)used);
	       return;
	  }
	  else {
	       have_names = 1;
	  }
     }

     flag_interrupts();
     if (!have_names) {
	  do_mtg("*");
     } else for (i = 1; i < argc; i++) {
	  if (!used[i])
	       do_mtg(argv[i]);
	  if (interrupt)
	       break;
     }
     checked_meetings = 1;

     if (print_header && !interrupt)
	  ss_perror(sci_idx, 0, "No changed meetings");
     
     free((char *)used);
     dont_flag_interrupts();
}

next_meeting(argc, argv)
     int argc;
     char **argv;
{
     name_blk *set;
     register name_blk *nbp;
     int n_matches, code, i;
     int ls_flag = 0;
     
     ++argv;
     --argc;
     
     while (argc) {
	     if (**argv == '-') {
		     if (!strcmp(*argv, "-list") || !strcmp(*argv, "-ls"))
			     ls_flag++;
		     else if(!strcmp(*argv,"-no_list")||!strcmp(*argv,"-nls"))
			     ls_flag = 0;
		     else {
			     ss_perror(sci_idx, 0,
				       "Unknown control argument.");
		     usage:
			     printf("Usage: nm [-list]\n");
			     return;
		     }
	     }
	     else
		     goto usage;
	     --argc;
	     ++argv;
     }
     
     dsc_expand_mtg_set(user_id, "*", &set, &n_matches, &code);

     if (code) {
	  ss_perror(sci_idx, code, "Can't get meeting names.");
	  return;
     }
     if (!n_matches) {
	  ss_perror(sci_idx, 0, "No meetings found.");
	  return;
     }
     print_header = 1;

     for (i = 0; i < n_matches; i++) {
	  nbp = &set[i];
	  if (nbp->status & DSC_ST_CHANGED) {
	       if (ls_flag) {
		    do_line(nbp, 0, 1);
	       } else {
	            switch_to_mtg_nb(nbp);
		    dsc_public.nb.status &= ~DSC_ST_CHANGED;
		    dsc_update_mtg_set(user_id, &dsc_public.nb, 1, &code);
		    if (code)
			 ss_perror(sci_idx, code,
				   "Error updating meetings file.");
		    goto done;
	       }
	  }
     }
     if (!checked_meetings && print_header) {
	  ss_perror(sci_idx, 0,
		    "No meetings listed as changed; use check_meetings.");
     } else if (print_header)
	  ss_perror(sci_idx, 0, "No more changed meetings.");
done:
     dsc_destroy_mtg_set(set, n_matches);
}
