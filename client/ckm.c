/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/ckm.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/ckm.c,v 1.8 1987-06-20 13:36:15 srz Exp $
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.7  87/06/14  21:08:44  srz
 * Added control-C handling.
 * 
 * Revision 1.6  87/06/14  16:28:24  srz
 * White space changes + changed fprintf into ss_perror.
 * 
 * Revision 1.5  87/04/19  22:17:18  srz
 * Reverted definition of 'changed' to include new meetings.
 * 
 * Revision 1.4  87/04/12  00:11:21  spook
 * Removed unused variables.
 * 
 * Revision 1.3  87/04/08  08:32:13  wesommer
 * Fixed the error message once again.
 * 
 * Revision 1.2  87/04/08  03:53:42  wesommer
 * Fixed error message.
 * 
 * Revision 1.1  87/03/24  14:11:09  spook
 * Initial revision
 * 
 */
     
#ifndef lint
static char *rcsid_ckm_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/ckm.c,v 1.8 1987-06-20 13:36:15 srz Exp $";
#endif lint

#include <strings.h>
#include <stdio.h>
#include "types.h"
#include "interface.h"
#include "globals.h"

extern char *malloc(),*ctime(), *error_message();

static int print_header, display;

static
do_mtg(mtg_name)
     char *mtg_name;
{
     name_blk *set;
     register name_blk *nbp;
     int n_matches, i, code;
     bool updated;
     char last_host[140], last_path[140];
     
     dsc_expand_mtg_set(user_id, mtg_name, &set, &n_matches, &code);
     if (!n_matches)
	  return (0);
     
     last_host[0] = '\0';
     last_path[0] = '\0';
     for (i = 0; i < n_matches; i++) {
	  if (interrupt)
	       break;
	  nbp = &set[i];
	  /* Test to see if we are attending this meeting */
	  if (dsc_public.attending && !strcmp(dsc_public.host, nbp ->hostname) && !strcmp(dsc_public.path, nbp->pathname)) {
	       updated = (dsc_public.highest_seen < dsc_public.m_info.last);
	  } else {
	       dsc_updated_mtg(nbp, &updated, &code);
	       if (interrupt)
		    break;
	       if (code) {
		    fprintf(stderr,
			    "Error checking meeting %s: %s\n",
			    nbp -> aliases[0],
			    error_message(code));
		    continue;
	       }
	  }
	  if (strcmp(last_host,nbp->hostname) || strcmp(last_path, nbp->pathname)) {
	       strcpy(last_host,nbp->hostname);
	       strcpy(last_path,nbp->pathname);
	       if (updated) {
		    nbp->status |= DSC_ST_CHANGED;
		    if (display) {
			 if (print_header) {
			      printf("   %-30s   %-30s\n",
				     "Meeting",
				     "Short name");
			      print_header = 0;
			 }
			 printf("   %-30s   %s\n",
				nbp->aliases[0],
				(nbp->aliases[1] ?
				 nbp->aliases[1] :
				 ""));
		    }
	       }
	       else {
		    nbp->status &= ~DSC_ST_CHANGED;
	       }
	  }
     }
     if (interrupt)
	  return(0);
     dsc_update_mtg_set(user_id, set, n_matches, &code);
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
	  if (*argv[i] == '-') {
	       ss_perror(sci_idx, 0,
			 sprintf(errbuf, "Unknown control argument %s\n",
				 argv[i]));
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
     }
     else for (i = 1; i < argc; i++) {
	  if (!used[i])
	       do_mtg(argv[i]);
	  if (interrupt)
	       break;
     }
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
     
     dsc_expand_mtg_set(user_id, "*", &set, &n_matches, &code);
     if (code) {
	  ss_perror(sci_idx, code, "Can't get meeting names.");
	  return;
     }
     if (!n_matches) {
	  ss_perror(sci_idx, 0, "No meetings found.");
	  return;
     }
     for (i = 0; i < n_matches; i++) {
	  nbp = &set[i];
	  if (nbp->status & DSC_ST_CHANGED) {
	       switch_to_mtg(nbp->aliases[0]);
	       dsc_public.nb.status &= ~DSC_ST_CHANGED;
	       dsc_update_mtg_set(user_id, &dsc_public.nb, 1, &code);
	       if (code)
		    ss_perror(sci_idx, code,
			      "Error updating meetings file.");
	       goto done;
	  }
     }
     ss_perror(sci_idx, 0, "No more changed meetings.");
 done:
     free(set);
}
