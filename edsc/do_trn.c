/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * do_trn.c -- Routines to implement the various lisp requests related
 * 		to transactions
 *
 */

#include <stdio.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <signal.h>
#include <sys/wait.h>
#include <ctype.h>
#include <sys/time.h>
#include <netdb.h>
#ifdef SVR4
#include <string.h>
#include <fcntl.h>
#else
#include <strings.h>
#endif
#include "edsc.h"

/*
 * Define POSIX-style macros for systems who don't have them.
 */
#ifndef S_ISREG
#define S_ISREG(m) ((m&S_IFMT) == S_IFREG)
#endif

do_gti(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     trn_info3 t_info;
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     dsc_get_trn_info3(&nb,trn_num,&t_info,&code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     t_info.subject = do_quote(t_info.subject);
     printf("(%d %d %d %d %d %d %d %d \"%s\" %d %d \"%s\" \"%s\" %d \"%s\")\n",
	    t_info.current,
	    t_info.prev,
	    t_info.next,
	    t_info.pref,
	    t_info.nref,
	    t_info.fref,
	    t_info.lref,
	    t_info.chain_index,
	    short_time(&t_info.date_entered),
	    t_info.num_lines,
	    t_info.num_chars,
	    t_info.subject,
	    t_info.author,
	    t_info.flags,
	    t_info.signature ? t_info.signature : "");
     
     dsc_destroy_name_blk(&nb);
     dsc_destroy_trn_info3(&t_info);
}

do_gt(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     trn_info3 tinfo;
     tfile tf;
     char *plural;
     char line[255];
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     dsc_get_trn_info3(&nb,trn_num,&tinfo,&code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     if (tinfo.num_lines != 1)
	  plural = "s";
     else
	  plural = "";
     

     if (tinfo.subject [0] == '\0')
	  tinfo.num_lines--;

     printf("(%d)\n",
	    tinfo.num_lines+3);
     fflush(stdout);		/* Flush out the number of lines */

     tf = stdout_tf;

     if (tinfo.signature && strcmp(tinfo.signature, tinfo.author))
	     (void) sprintf (line, "[%04d]%c %s (%s) %s %s (%d line%s)\n",
			     tinfo.current,
			     tinfo.flags & TRN_FLAG1 ? 'F' : ' ',
			     tinfo.author, tinfo.signature, mtg_name,
			     short_time (&tinfo.date_entered),
			     tinfo.num_lines, plural);
     else
	     (void) sprintf (line, "[%04d]%c %s %s %s (%d line%s)\n",
			     tinfo.current,
			     tinfo.flags & TRN_FLAG1 ? 'F' : ' ',
			     tinfo.author, mtg_name,
			     short_time (&tinfo.date_entered),
			     tinfo.num_lines, plural);
     twrite (tf, line, strlen (line), &code);
     if (tinfo.subject [0] != '\0') {
	  twrite (tf, "Subject: ", 9, &code);
	  twrite (tf, tinfo.subject, strlen (tinfo.subject), &code);
	  twrite (tf, "\n", 1, &code);
     }

     dsc_get_trn(&nb, trn_num, tf, &code);
     if (code != 0) return;

     if (tinfo.pref == 0 && tinfo.nref == 0)
	  (void) sprintf (line, "--[%04d]--\n", tinfo.current);
     else if (tinfo.pref == 0)
	  (void) sprintf (line, "--[%04d]-- (nref = [%04d])\n",
			  tinfo.current, tinfo.nref);
     else if (tinfo.nref == 0)
	  (void) sprintf (line, "--[%04d]-- (pref = [%04d])\n",
			  tinfo.current, tinfo.pref);
     else
	  (void) sprintf (line,
			  "--[%04d]-- (pref = [%04d], nref = [%04d])\n",
			  tinfo.current, tinfo.pref, tinfo.nref);
     twrite (tf, line, strlen (line), &code);

     dsc_destroy_name_blk(&nb);
     dsc_destroy_trn_info3(&tinfo);
}

do_gtf(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string, *output_fn;
     trn_nums trn_num;
     name_blk nb;
     trn_info3 tinfo;
#ifndef EDSC_CACHE
     tfile tf;
     int fd;
     char *plural;
     char line[255];
#else
     struct stat stat_buf;
#endif
     int code;

     /* First, get the output filename */
     if (get_word(&cp, &output_fn, " ", &delim) < 0) {
	  printf(";Missing output filename\n");
	  return;
     }

     /* Now, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

#ifdef EDSC_CACHE
     while (!cache_transaction(&nb, trn_num, &cache_current))
	     ;
     if (cache_current->info_code) {
	     printf(";%s\n", error_message(cache_current->info_code));
	     dsc_destroy_name_blk(&nb);
	     return;
     }
     if (cache_current->text_code) {
	     printf(";%s\n", error_message(cache_current->info_code));
	     dsc_destroy_name_blk(&nb);
	     return;
     }
     cache_pc = 0;
     cache_working = 1;
     tinfo = cache_current->t_info;
     if ((!stat(output_fn, &stat_buf)) &&
	 S_ISREG(stat_buf.st_mode))
	     (void) unlink(output_fn);
     if (link(cache_current->filename, output_fn)) {
	     int	fd, rfd, cc;
	     char	buf[8192];
	     
	     if ((fd = open(output_fn,
			    O_WRONLY | O_TRUNC | O_CREAT, 0600)) < 0) {
		     printf(";Can't open output file %s: %s\n", output_fn,
			    error_message(errno));
		     dsc_destroy_name_blk(&nb);
		     return;
	     }
	     if ((rfd = open(cache_current->filename, O_RDONLY, 0)) < 0) {
		     printf(";Can't open input file %s: %s\n", output_fn,
			    error_message(errno));
		     dsc_destroy_name_blk(&nb);
		     return;
	     }
	     while ((cc = read(rfd, buf, sizeof(buf))) > 0) {
		     if (cc != write(fd, buf, cc)) {
			     printf("; Failed write!\n");
			     dsc_destroy_name_blk(&nb);
			     return;
		     }
	     }
	     if (cc < 0) {
		     printf("; Failed read!\n");
		     dsc_destroy_name_blk(&nb);
		     return;
	     }
     }

#else
     if ((fd = open(output_fn, O_WRONLY | O_TRUNC | O_CREAT, 0600)) < 0) {
	     printf(";Can't open %s: %s\n", output_fn, error_message(errno));
	     return;
     }
     tf = unix_tfile(fd);

     dsc_get_trn_info3(&nb,trn_num,&tinfo,&code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     if (tinfo.num_lines != 1)
	  plural = "s";
     else
	  plural = "";
     

     if (tinfo.subject [0] == '\0')
	  tinfo.num_lines--;

     if (tinfo.signature && strcmp(tinfo.signature, tinfo.author))
	     (void) sprintf (line, "[%04d]%c %s (%s) %s %s (%d line%s)\n",
			     tinfo.current,
			     tinfo.flags & TRN_FLAG1 ? 'F' : ' ',
			     tinfo.author, tinfo.signature, mtg_name,
			     short_time (&tinfo.date_entered),
			     tinfo.num_lines, plural);
     else
	     (void) sprintf (line, "[%04d]%c %s %s %s (%d line%s)\n",
			     tinfo.current,
			     tinfo.flags & TRN_FLAG1 ? 'F' : ' ',
			     tinfo.author, mtg_name,
			     short_time (&tinfo.date_entered),
			     tinfo.num_lines, plural);
     twrite (tf, line, strlen (line), &code);
     if (tinfo.subject [0] != '\0') {
	  twrite (tf, "Subject: ", 9, &code);
	  twrite (tf, tinfo.subject, strlen (tinfo.subject), &code);
	  twrite (tf, "\n", 1, &code);
     }

     dsc_get_trn(&nb, trn_num, tf, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     if (tinfo.pref == 0 && tinfo.nref == 0)
	  (void) sprintf (line, "--[%04d]--\n", tinfo.current);
     else if (tinfo.pref == 0)
	  (void) sprintf (line, "--[%04d]-- (nref = [%04d])\n",
			  tinfo.current, tinfo.nref);
     else if (tinfo.nref == 0)
	  (void) sprintf (line, "--[%04d]-- (pref = [%04d])\n",
			  tinfo.current, tinfo.pref);
     else
	  (void) sprintf (line,
			  "--[%04d]-- (pref = [%04d], nref = [%04d])\n",
			  tinfo.current, tinfo.pref, tinfo.nref);
     twrite (tf, line, strlen (line), &code);

     tclose(tf, &code);
     (void) close(fd);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }
#endif

     tinfo.subject = do_quote(tinfo.subject);
     printf("(%d %d %d %d %d %d %d %d \"%s\" %d %d \"%s\" \"%s\" %d)\n",
	    tinfo.current,
	    tinfo.prev,
	    tinfo.next,
	    tinfo.pref,
	    tinfo.nref,
	    tinfo.fref,
	    tinfo.lref,
	    tinfo.chain_index,
	    short_time(&tinfo.date_entered),
	    tinfo.num_lines,
	    tinfo.num_chars,
	    tinfo.subject,
	    tinfo.author,
	    tinfo.flags);
     
     dsc_destroy_name_blk(&nb);
#ifndef EDSC_CACHE
     dsc_destroy_trn_info3(&tinfo);
#endif
}

#ifdef EDSC_CACHE
do_gtfc(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string, *dir_string;
     trn_nums trn_num;
     int direction;
     name_blk nb;
     trn_info3 tinfo;
     int code;

     /* First get the direction number */
     if (get_word(&cp, &dir_string, " ", &delim) < 0) {
	  printf(";Missing direction number\n");
	  return;
     }

     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     direction = atoi(dir_string);
     
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     while (!cache_transaction(&nb, trn_num, &cache_current))
	     ;
     if (cache_current->info_code) {
	     printf(";%s\n", error_message(cache_current->info_code));
	     dsc_destroy_name_blk(&nb);
	     return;
     }
     if (cache_current->text_code) {
	     printf(";%s\n", error_message(cache_current->text_code));
	     dsc_destroy_name_blk(&nb);
	     return;
     }
     cache_pc = 0;
     cache_working = 1;
     tinfo = cache_current->t_info;
     
     tinfo.subject = do_quote(tinfo.subject);
     printf("(\"%s\" %d %d %d %d %d %d %d %d \"%s\" %d %d \"%s\" \"%s\" %d)\n",
	    cache_current->filename,
	    tinfo.current,
	    tinfo.prev,
	    tinfo.next,
	    tinfo.pref,
	    tinfo.nref,
	    tinfo.fref,
	    tinfo.lref,
	    tinfo.chain_index,
	    short_time(&tinfo.date_entered),
	    tinfo.num_lines,
	    tinfo.num_chars,
	    tinfo.subject,
	    tinfo.author,
	    tinfo.flags);
     
     dsc_destroy_name_blk(&nb);
     if (direction > 0 && direction <= 4)
	     cache_current_direction = direction;
}
#endif /* EDSC_CACHE */

do_grt(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     trn_info tinfo;
     tfile tf;
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     dsc_get_trn_info(&nb,trn_num,&tinfo,&code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     printf("(%d)\n",
	    tinfo.num_chars);

     tf = stdout_tf;
     dsc_get_trn(&nb, trn_num, tf, &code);

     dsc_destroy_name_blk(&nb);
     dsc_destroy_trn_info3(&tinfo);
}

do_at(args)
char *args;
{
     char *cp = args, *mtg_name, *file_name, delim, *trn_string;
     trn_nums trn_num,new_trn_num;
     name_blk nb;
     char subject[1000];
     tfile tf;
     int fd, code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &file_name, " ", &delim) < 0) {
	  printf(";Missing file name\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     /* Read the subject */
     if (gets(subject) == NULL) {
	  printf(";End of file\n");
	  exit(1);
     }

     fd = open(file_name, O_RDONLY, 0);
     if (fd < 0) {
	  printf(";Can't read transaction.  %s\n", error_message(errno));
	  goto punt;
     }

     tf = unix_tfile(fd);
     dsc_add_trn(&nb, tf, subject, trn_num, &new_trn_num, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
     } else {
	  printf("(%d)\n", new_trn_num);
     }

     close(fd);
#ifdef EDSC_CACHE
     cache_itn(&nb, new_trn_num);
#endif
punt:
     dsc_destroy_name_blk(&nb);
}

do_nut(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num,cur_trn;
     name_blk nb;
     mtg_info m_info;
     trn_info t_info;
     int code,i;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     dsc_get_mtg_info (&nb, &m_info, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     if (trn_num < m_info.first) {
	  printf("(%d)\n", m_info.first);
	  goto done2;
     }
     if (trn_num > m_info.last) {
	  printf("(0)\n");
	  goto done2;
     }

     /* Now we try to find the next transaction.  If the transaction
	is not deleted, we can do this in one step */
     dsc_get_trn_info(&nb,trn_num,&t_info,&code);
     if (code == 0) {
	  printf("(%d)\n", t_info.next);
	  goto done;
     }

     /* Hmm.  Deleted transaction.  Try the next five transactions, hoping
	to get a non-deleted one */
     for (i = 1; i <= 5; i++) {
	  dsc_get_trn_info(&nb,trn_num+i,&t_info,&code);
	  if (code == 0) {
	       printf("(%d)\n", t_info.current);
	       goto done;
	  }
     }

     /* Hmmm.  Try the first 5 transactions in the meeting, in case we
	hit a big gap after transaction [0001]. */
     cur_trn = m_info.first;
     for (i = 0; i < 5; i++) {
	  dsc_get_trn_info(&nb, cur_trn, &t_info, &code);
	  if (code != 0) {
	       printf(";%s\n", error_message(code));
	       goto done2;
	  }
	  if (t_info.next > trn_num) {
	       printf("(%d)\n", t_info.next);
	       goto done;
	  }
	  cur_trn = t_info.next;
	  dsc_destroy_trn_info(&t_info);
     }

     /* Ok.  We bite the bullet and loop until we can find something. */
     code = DELETED_TRN;
     trn_num += 6;
     while (code == DELETED_TRN) {
	  dsc_get_trn_info (&nb, trn_num, &t_info, &code);
	  if (code == 0) {
	       printf("(%d)\n", trn_num);
	       goto done;
	  } else if (code == DELETED_TRN)
	       trn_num++;
	  else {
	       printf(";%s\n", error_message(code));
	       goto done2;
	  }
     }

done:
     dsc_destroy_trn_info(&t_info);
done2:     
     dsc_destroy_name_blk(&nb);
     dsc_destroy_mtg_info(&m_info);
}

do_grtn(args)
char *args;
{
     char *cp = args, *mtg_name, delim;
     name_blk nb;
     mtg_info m_info;
     trn_info3 t_info;
     int code, rnd_num, i;
     int randrp_retry = 15;
     struct timeval tv;
     int pid = getpid();
     int active_transactions, rnd_trn;

     /* First, we get the meeting name */
     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     dsc_get_mtg_info(&nb,&m_info,&code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     gettimeofday(&tv, (struct timezone *) NULL);
     srandom(tv.tv_sec ^ tv.tv_usec ^ pid);

     for (i=1;i<=randrp_retry;i++) {
	     do {
		     rnd_num = random();
		     active_transactions = (m_info.last - m_info.first);
		     if (active_transactions != 0) {
			     rnd_trn = (m_info.first +
					(rnd_num % active_transactions));
		     } else {
			     rnd_trn = m_info.first;
		     }
		     dsc_get_trn_info3(&nb, rnd_trn, &t_info, &code);
	     } while (code != 0);
	     if (!t_info.pref) break;
     }
     
     t_info.subject = do_quote(t_info.subject);
     printf("(%d %d %d %d %d %d %d %d \"%s\" %d %d \"%s\" \"%s\" %d \"%s\")\n",
	    t_info.current,
	    t_info.prev,
	    t_info.next,
	    t_info.pref,
	    t_info.nref,
	    t_info.fref,
	    t_info.lref,
	    t_info.chain_index,
	    short_time(&t_info.date_entered),
	    t_info.num_lines,
	    t_info.num_chars,
	    t_info.subject,
	    t_info.author,
	    t_info.flags,
	    t_info.signature ? t_info.signature : "");
     
     dsc_destroy_name_blk(&nb);
     dsc_destroy_mtg_info(&m_info);
}

do_sfl(args)
	char	*args;
{
     char *cp = args, *mtg_name, delim, *trn_string, *flags_string;
     trn_nums trn_num;
     int	flags;
     name_blk 	nb;
     int	code;
     
     /* First, we get flag we should be setting */
     if (get_word(&cp, &flags_string, " ", &delim) < 0) {
	  printf(";Missing flags entry\n");
	  return;
     }

     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }
     trn_num = atoi(trn_string);
     flags = atoi(flags_string);
     
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     dsc_set_trn_flags(&nb, trn_num, flags, &code);

#ifdef EDSC_CACHE
     cache_it(&nb, trn_num);
#endif
     
     dsc_destroy_name_blk(&nb);
     
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     printf("()\n");
}

/*
 * Retrieve transaction
 */
do_rt(args)
	char	*args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }
     dsc_retrieve_trn(&nb, trn_num, &code);
#ifdef EDSC_CACHE
     if (!code)
	     cache_itn(&nb, trn_num);
#endif
     dsc_destroy_name_blk(&nb);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }
     printf("()\n");


}

/*
 * Delete transaction
 */
do_dt(args)
	char	*args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }
     
     dsc_delete_trn(&nb, trn_num, &code);
#ifdef EDSC_CACHE
     if (!code)
	     cache_itn(&nb, trn_num);
#endif
     dsc_destroy_name_blk(&nb);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }
     printf("()\n");

}
