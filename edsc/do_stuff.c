/*
 *
 * do_stuff () -- Routines to implement the various lisp requests.
 *
 *	Copyright (C) 1988 by the Student Information Processing Board
 *
 */

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include <ctype.h>
#include "discuss.h"

extern char *user_id;
extern tfile stdout_tf;
extern int bit_bucket();
extern int errno;
extern char *do_quote();
tfile unix_tfile();

do_gmi(args)
char *args;
{
     char *cp = args, *mtg_name, delim;
     name_blk nb;
     mtg_info m_info;
     char mtime[30];
     int code;

     /* First, we get the meeting name */
     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf("; Missing meeting name\n");
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

     strcpy(mtime, short_time(&m_info.date_created));
     printf("(\"%s\" \"%s\" \"%s\" %d %d %d %d \"%s\" \"%s\" %d \"%s\" %d)\n",
	    m_info.location,
	    m_info.long_name,
	    m_info.chairman,
	    m_info.first,
	    m_info.last,
	    m_info.lowest,
	    m_info.highest,
	    mtime,
	    short_time(&m_info.date_modified),
	    m_info.public_flag,
	    m_info.access_modes,
	    nb.last);
     
     dsc_destroy_name_blk(&nb);
     dsc_destroy_mtg_info(&m_info);
}

do_gti(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     trn_info t_info;
     char mtime[30];
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf("; Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf("; Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     dsc_get_trn_info(&nb,trn_num,&t_info,&code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     t_info.subject = do_quote(t_info.subject);
     printf("(%d %d %d %d %d %d %d %d \"%s\" %d %d \"%s\" \"%s\")\n",
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
	    t_info.author);
     
     dsc_destroy_name_blk(&nb);
     dsc_destroy_trn_info(&t_info);
}

do_gt(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     trn_info tinfo;
     tfile tf;
     char *plural;
     char newtime[30],line[255];
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf("; Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf("; Missing meeting name\n");
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

     if (tinfo.num_lines != 1)
	  plural = "s";
     else
	  plural = "";
     

     if (tinfo.subject [0] == '\0')
	  tinfo.num_lines--;

     printf("(%d)\n",
	    tinfo.num_lines+3);

     tf = stdout_tf;

     (void) sprintf (line, "[%04d] %s %s %s (%d line%s)\n",
		     tinfo.current, tinfo.author, mtg_name,
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
     dsc_destroy_trn_info(&tinfo);
}

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
	  printf("; Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf("; Missing meeting name\n");
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
     dsc_destroy_trn_info(&tinfo);
}

do_gml(args)
char *args;
{
     name_blk *set,*nbp;
     char **aliases;
     int code, n_matches, i;
     int first;
     bool updated;

     dsc_expand_mtg_set(user_id, "*", &set, &n_matches, &code);
     if (code) {
	  printf(";%s", error_message(code));
	  return;
     }

     set_warning_hook(bit_bucket);
     putchar('(');

     first = TRUE;
     for (i = 0; i < n_matches; i++) {
	  nbp = &set[i];

	  dsc_updated_mtg(nbp, &updated, &code);
	  if (code != 0) {	/* meeting lost trns */
	       updated = TRUE;
	       code = 0;
	  }

	  if (first) {
	       first = FALSE;
	  } else
	       putchar(' ');

	  printf("(%d", updated ? 1 : 0);
	  aliases = nbp -> aliases;
	  while (*aliases) {
	       printf(" \"%s\"", *(aliases++));
	  }

	  printf(")");
     }

     printf(")\n");
     dsc_destroy_mtg_set(set, n_matches);
}

do_gcm(args)
char *args;
{
     name_blk *set,*nbp;
     char **aliases;
     int code, n_matches, i;
     int first;
     bool updated;

     dsc_expand_mtg_set(user_id, "*", &set, &n_matches, &code);
     if (code) {
	  printf(";%s", error_message(code));
	  return;
     }

     set_warning_hook(bit_bucket);
     putchar('(');

     first = TRUE;
     for (i = 0; i < n_matches; i++) {
	  nbp = &set[i];

	  dsc_updated_mtg(nbp, &updated, &code);
	  if (code != 0) {	/* meeting lost trns */
	       updated = TRUE;
	       code = 0;
	  }

	  if (!updated)
	       continue;

	  if (first) {
	       first = FALSE;
	  } else
	       putchar(' ');

	  printf("(%d", updated ? 1 : 0);
	  aliases = nbp -> aliases;
	  while (*aliases) {
	       printf(" \"%s\"", *(aliases++));
	  }

	  printf(")");
     }

     printf(")\n");
     dsc_destroy_mtg_set(set, n_matches);
}

do_ss(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     trn_info t_info;
     char mtime[30];
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf("; Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf("; Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     nb.last = trn_num;
     dsc_update_mtg_set (user_id, &nb, 1, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }

     dsc_destroy_name_blk(&nb);
}

do_at(args)
char *args;
{
     char *cp = args, *mtg_name, *file_name, delim, *trn_string;
     trn_nums trn_num,new_trn_num;
     name_blk nb;
     trn_info t_info;
     char mtime[30];
     char subject[1000];
     tfile tf;
     int fd, code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf("; Missing trn number\n");
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
     char mtime[30];
     int code,i;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf("; Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf("; Missing meeting name\n");
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
