/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * pmtg.c  -- Program to print out a entire meeting.
 *
 */

#include <discuss/discuss.h>
#include <sys/file.h>
#include <stdio.h>
#include <string.h>

tfile unix_tfile();
mtg_info minfo;

#ifndef	lint
static char rcsid_pmtg_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/pmtg.c,v 1.4 1994-03-25 16:57:12 miki Exp $";
#endif

int main (argc,argv)
	int argc;
	char **argv;
{
	int result;
	trn_nums trn;
	trn_info tinfo;
	tfile tfstdout;
	char machine [50],mtg_name[100];

	init_dsc_err_tbl();
	argc--; argv++;
	if (argc != 1)
		goto lusage;

	resolve_mtg(*argv, machine, mtg_name);

	init_rpc();
	if (open_rpc (machine, "discuss", &result) == 0) {
	     (void) fprintf (stderr, "%s\n", error_message(result));
	     exit(1);
	}
	if (result) {
	     (void) fprintf (stderr, "Warning: %s\n", error_message(result));
	}

	get_mtg_info (mtg_name, &minfo, &result);
	if (result != 0) {
		(void) fprintf (stderr, "%s\n", error_message (result));
		exit(1);
	}

	/* set up stdout tfile */
	tfstdout = unix_tfile (1);

	trn = minfo.first;
	while (trn != 0) {
		get_trn_info (mtg_name, trn, &tinfo, &result);
		if (result != 0) {
			if (result != DELETED_TRN) {
				(void) fprintf (stderr, "%s\n",
						error_message (result));
				exit (1);
			}
		} else {
			write_header (&tinfo, tfstdout);
			get_trn (mtg_name, trn, tfstdout, &result);
			if (result != 0) {
				(void) fprintf (stderr, "%s\n",
						error_message(result));
				exit (1);
			}
			write_trailer (&tinfo, tfstdout);
		}
		trn = tinfo.next;
		free (tinfo.author);
		free (tinfo.subject);
	}

	tdestroy (tfstdout);
	term_rpc ();
	return 0;

 lusage:
	(void) fprintf (stderr, "Usage: pmtg {mtg_name}\n");
	exit (1);
}

char *ctime();

write_header(info, tf)
	trn_info *info;
	tfile tf;
{
	char line [255];
	char newtime [26];
	char *plural;
	int dummy;

	(void) strcpy (newtime, ctime (&(info -> date_entered)));
	newtime [24] = '\0';	/* get rid of \n */

	if (info -> num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	(void) sprintf (line, "[%04d] %s %s %s (%d line%s)\n",
			info -> current, info -> author, minfo.long_name,
			&newtime[4], info -> num_lines, plural);
	twrite (tf, line, strlen (line),&dummy);
	if (info -> subject [0] != '\0') {
		twrite (tf, "Subject: ", 9, &dummy);
		twrite (tf, info -> subject, strlen (info -> subject), &dummy);
		twrite (tf, "\n", 1, &dummy);
	}
	return;
}

write_trailer (info, tf)
trn_info *info;
tfile tf;
{
     char line [255];
     int dummy;

     if (info -> pref == 0 && info -> nref == 0)
	  sprintf (line, "--[%04d]--\n\n", info -> current);
     else if (info -> pref == 0)
	  sprintf (line, "--[%04d]-- (nref = [%04d])\n\n", info -> current,
		   info -> nref);
     else if (info -> nref == 0)
	  sprintf (line, "--[%04d]-- (pref = [%04d])\n\n", info -> current,
		   info -> pref);
     else
	  sprintf (line, "--[%04d]-- (pref = [%04d], nref = [%04d])\n\n",
		   info -> current, info -> pref, info -> nref);
     twrite (tf, line, strlen (line),&dummy);
}
/*
 *
 * resolve_mtg:  Procedure to resolve a user meeting name into its host
 * 	         an pathname.
 *
 */
resolve_mtg (usr_string, machine, mtg_name)
char *usr_string,*machine,*mtg_name;
{
     char *colon;
     int machine_len;

     colon = strchr (usr_string, ':');

     if (colon == 0) {
	  strcpy (mtg_name, usr_string);
	  gethostname (machine, 50);
	  return;
     }

     machine_len = colon - usr_string;
#ifdef POSIX
     memmove (machine, usr_string, machine_len);
#else
     bcopy (usr_string, machine, machine_len);
#endif
     machine [machine_len] = '\0';
     strcpy (mtg_name, colon+1);
     return;
}
