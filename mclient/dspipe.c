/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * dspipe.c -- Program to pipe stdin into a Discuss meeting.
 *
 */

#include <discuss/tfile.h>
#include <discuss/interface.h>
#include <discuss/dsc_et.h>
#include <rpc.h>
#include <sys/file.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

tfile unix_tfile();
char *mktemp();

#ifndef	lint
static char rcsid[] = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/dspipe.c,v 1.8 1996-09-19 22:31:29 ghudson Exp $";
#endif

main (argc,argv)
int argc;
char **argv;
{
	char *usr_mtg = "";
	char *subject = "";
	char *signature = "";
	char mtg_name [100],host[70];
	int result;
	int fatal_err;
	trn_nums result_trn;
	char filename [40], buffer [512];
	tfile tf;
	int i,nread,d,exit_code;

	init_dsc_err_tbl();
	for (i = 1; i < argc; i++) {
		if (*argv[i] == '-')
			switch (argv[i][1]) {
			case 'd':
			case 'a':
				continue;
				
			case 't':
				if (++i < argc)
					subject = argv[i];
				continue;

			case 's':
				if (++i < argc)
					signature = argv[i];
				continue;

			default:
				goto lusage;
			}
		if (*usr_mtg != '\0')
			goto lusage; /* only one meeting name */
		
		usr_mtg = argv[i];
	}
	
	if (*usr_mtg == '\0')
		goto lusage;


	(void) strcpy (filename, "/tmp/DSXXXXXX");
	(void) mktemp (filename);

	d = open (filename, O_RDWR | O_CREAT, 0700);
	if (d < 0) {
		(void) fprintf (stderr, "Cannot open temp file\n");
		exit (1);
	}

	for (;;) {
		nread = read (0, buffer, 512);
		if (nread < 0) {
			(void) fprintf (stderr, "Cannot read stdin\n");
		}
		write (d, buffer, nread);
		if (nread == 0)
			break;
	}

	lseek (d, 0, 0);	/* rewind temp */

	tf = unix_tfile (d);

	strcpy (host, "discuss@");
	resolve_mtg (usr_mtg, &host[8], mtg_name);

	init_rpc();
	set_module (host, &fatal_err, &result);
	if (fatal_err) {
	     (void) fprintf (stderr, "%s\n", error_message(result));
	     exit(1);
	}
	if (result) {
	     (void) fprintf (stderr, "Warning: %s\n", error_message(result));
	}

	if (signature[0] == '\0') {
	     add_trn (mtg_name, tf, subject, 0, &result_trn, &result);
	} else {
	     if (get_server_version() < SERVER_2)
		  add_trn (mtg_name, tf, subject, 0, &result_trn, &result);
	     else
		  add_trn2(mtg_name, tf, subject, signature, 0, &result_trn, &result);
	}
		  
	if (result != 0) {
		(void) fprintf (stderr, "%s\n", error_message(result));
		exit_code = 1;
		goto bye;
	}

	(void) printf ("Transaction [%04d] added to %s\n", result_trn,
		       mtg_name);
	exit_code = 0;

 bye:
	(void) close (d);
	tdestroy (tf);
	(void) unlink (filename);

	term_rpc();
	exit (exit_code);

 lusage:
	(void) fprintf (stderr, "Usage: dspipe {mtgname} -t {topic} -s {signature}\n");
	exit (1);
}

/*
 *
 * resolve_mtg:  Procedure to resolve a user meeting name into its host
 * 	         an pathname.
 *
 */
resolve_mtg (usr_string, host, mtg_name)
char *usr_string,*host,*mtg_name;
{
	char *colon;
	int host_len;

	colon = strchr (usr_string, ':');

	if (colon == 0) {
		(void) strcpy (mtg_name, usr_string);
		gethostname (host, 50);
		return;
	}

	host_len = colon - usr_string;
#ifdef POSIX
      memmove (host, usr_string, host_len);
#else
	bcopy (usr_string, host, host_len);
#endif
	host [host_len] = '\0';
	(void) strcpy (mtg_name, colon+1);
	return;
}
