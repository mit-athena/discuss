/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * catchup request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/catchup.c,v 1.6 1996-09-08 20:31:05 ghudson Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/catchup.c,v $
 * $Locker:  $
 *
 */
#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/catchup.c,v 1.6 1996-09-08 20:31:05 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <string.h>
#include <discuss/discuss.h>
#include <ss/ss.h>
#include "config.h"
#include "globals.h"

catchup(argc, argv)
	int argc;
	char **argv;
{
	int	code;
	
	if (!dsc_public.attending) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		return;
	}
	dsc_destroy_mtg_info(&dsc_public.m_info);
	dsc_get_mtg_info(&dsc_public.nb,
			 &dsc_public.m_info, &code);
	if (code != 0) {
		(void) ss_perror(sci_idx, code, "Can't get meeting info");
		return;
	}

	dsc_public.highest_seen = dsc_public.m_info.highest;
	dsc_public.current = dsc_public.highest_seen;
}

set_seen(argc, argv)
     int argc;
     char **argv;
{
     int code;
     trn_nums set_trn;
     selection_list *trn_list;
     trn_info t_info;
	
     if (argc != 2) {
	  fprintf(stderr, "Usage: set seen <transaction>\n");
	  return;
     }

     if (!dsc_public.attending) {
	  ss_perror(sci_idx, 0, "No current meeting.\n");
	  return;
     }

     dsc_destroy_mtg_info(&dsc_public.m_info);
     dsc_get_mtg_info(&dsc_public.nb, &dsc_public.m_info, &code);
     if (code != 0) {
	  (void) ss_perror(sci_idx, code, "Can't get meeting info");
	  return;
     }

     dsc_get_trn_info(&dsc_public.nb, dsc_public.current, &t_info, &code);
     if (code != 0)
	  t_info.current = 0;
     dsc_destroy_trn_info(&t_info);

     trn_list = trn_select(&t_info, argv[1],
			      (selection_list *)NULL, &code);
     if (code) {
	  ss_perror(sci_idx, code, "");
	  sl_free(trn_list);
	  return;
     }
     
     if (trn_list -> low != trn_list -> high) {
	  ss_perror(sci_idx, 0, "Cannot set seen to range");
	  sl_free(trn_list);
	  return;
     }
     
     set_trn = trn_list -> low;
     sl_free(trn_list);

     dsc_public.highest_seen = set_trn;
     dsc_public.current = dsc_public.highest_seen;
}
