/*
 *
 * catchup request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/catchup.c,v 1.1 1988-03-13 03:31:46 tytso Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/catchup.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1988 by the MIT Student Information Processing Board
 *
 */
#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/catchup.c,v 1.1 1988-03-13 03:31:46 tytso Exp $";
#endif lint

#include <stdio.h>
#include <strings.h>
#include "discuss_err.h"
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"
#include "dsc_et.h"
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
	dsc_get_mtg_info(&dsc_public.nb,
			 &dsc_public.m_info, &code);
	if (code != 0) {
		(void) ss_perror(sci_idx, code, "Can't get meeting info");
		return;
	}

	dsc_public.highest_seen = dsc_public.m_info.highest;
	dsc_public.current = dsc_public.highest_seen;
}

