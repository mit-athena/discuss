#include <stdio.h>
#include <sys/file.h>
#include <strings.h>
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"

extern ss_request_table discuss_cmds;
extern int current_trans;
extern char *cur_mtg;
extern int l_zcode;
extern char *temp_file;
extern char *pgm;
extern char *malloc(), *getenv();
extern mtg_info m_info;
extern char buffer[BUFSIZ];

write_trans(sci_idx, txn_no, tf)
	int sci_idx;
	trn_nums txn_no;
	tfile tf;
{
	char *plural;
	char newtime[26];
	char line[255];
	trn_info tinfo;

	get_trn_info(cur_mtg, txn_no, &tinfo, &l_zcode);
	if (l_zcode != 0) {
		if (l_zcode == DELETED_TRN)
			fprintf(stderr, "Transaction has been deleted.\n");
		else if (l_zcode == NO_SUCH_TRN)
			fprintf(stderr, "No such transaction.\n");
		else
			fprintf(stderr, "Error %d.\n", l_zcode);
		ss_abort_line(sci_idx);
	}
	strcpy (newtime, ctime (&(tinfo.date_entered)));
	newtime [24] = '\0';			/* get rid of \n */

	if (tinfo.num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	sprintf (line, "[%04d] %s %s %s (%d line%s)\n",
		 tinfo.current, tinfo.author, m_info.long_name, &newtime[4],
		 tinfo.num_lines, plural);
	twrite (tf, line, strlen (line));
	if (tinfo.subject [0] != '\0') {
		twrite (tf, "Subject: ", 9);
		twrite (tf, tinfo.subject, strlen (tinfo.subject));
		twrite (tf, "\n", 1);
	}
	get_trn(cur_mtg, txn_no, tf, &l_zcode);
	if (l_zcode != 0) {
		fprintf(stderr, "Error %d.\n", l_zcode);
		ss_abort_line(sci_idx);
	}
	if (tinfo.pref == 0 && tinfo.nref == 0)
		sprintf (line, "--[%04d]--\n\n", tinfo.current);
	else if (tinfo.pref == 0)
		sprintf (line, "--[%04d]-- (nref = [%04d])\n\n", tinfo.current,
			 tinfo.nref);
	else if (tinfo.nref == 0)
		sprintf (line, "--[%04d]-- (pref = [%04d])\n\n", tinfo.current,
			 tinfo.pref);
	else
		sprintf (line, "--[%04d]-- (pref = [%04d], nref = [%04d])\n\n",
			 tinfo.current, tinfo.pref, tinfo.nref);
	twrite (tf, line, strlen (line));
}

display_file(sci_idx, fn)
	int sci_idx;
	char *fn;
{
	int pid;
	switch((pid = fork())) {
	case -1:
		/* oops */
		ss_perror(sci_idx, ERRNO, "Can't fork.");
		ss_abort_line(sci_idx);
	case 0:
		/* child */
		chmod(fn, 0400);
		execlp("more", "more", fn, (char *)NULL);
		ss_perror(sci_idx, ERRNO, "Can't exec 'more'.");
		exit(1);
	default:
		/* parent */
		while(wait(0) != pid) {
			/* nil */
		};
	}
}
