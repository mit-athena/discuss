/*
 *
 *	Copyright (C) 1988, 1989, 1991 by the Massachusetts Institute of
 * 	Technology.
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * recover -- program to recover a meeting from the transaction file.
 *	      this program is linked to a server so it can use the
 *	      privileged procedures of create_mtg, and the like.
 *
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#ifdef SOLARIS
#include <fcntl.h>
#endif
#include <discuss/types.h>
#include <discuss/dsc_et.h>
#include <discuss/tfile.h>
#include <discuss/interface.h>
#include "mtg.h"

#define NULL 0
#define MAX_TRNS 20000
#define min(a, b) (a < b ? a : b)

static trn_base tb;
static trn_hdr th;
static int tempf;
static int trnf;
static int trnfsize;
static int num_trns;
static int current_pos;
static char *mtg_name, *location, *chairman, *trn_file;
static int trn_pos[MAX_TRNS];
static int found_eof;
static int do_byteswap;
static char *temp_dir = "/tmp";

tfile unix_tfile ();
char *malloc();
static fsize(),read_trn_hdr(),read_last_trn(),save_trn();

extern char rpc_caller[];
extern int has_privs, use_zephyr;

#ifndef __GNUC__
#define inline
#endif

#ifndef SOLARIS
static inline short Sshort(P_s)
#else
static  short Sshort(P_s)
#endif
	short	P_s;
{
    union {
	short s;
	char c[2];
    } x1, x2;
    x1.s = P_s;
    x2.c[0] = x1.c[1];
    x2.c[1] = x1.c[0];
    return x2.s;
}

#ifndef SOLARIS
static inline long Slong(P_l)
#else
static long Slong(P_l)
#endif
	long	P_l;
{
    union {
	long l;
	char c[4];
    } x1, x2;
    x1.l = P_l;
    x2.c[0] = x1.c[3];
    x2.c[1] = x1.c[2];
    x2.c[2] = x1.c[1];
    x2.c[3] = x1.c[0];
    return x2.l;
}

#define S(X) \
    (sizeof(X)==4		\
     ? (X = Slong(X))		\
     : (sizeof(X)==2		\
	? (X = Sshort(X))	\
	: (sizeof(X)==1		\
	   ? 0			\
	   : abort())))

main (argc, argv)
int argc;
char **argv;
{
     int i;

     has_privs = TRUE;
     use_zephyr = 0;

     init_dsc_err_tbl();

     for (i = 1; i < argc; i++) {
	  if (*argv[i] == '-') switch (argv[i][1]) {
	  case 'c':
	       if (++i < argc)
		    chairman = argv[i];
	       continue;

	  case 'n':
	       if (++i < argc)
		    mtg_name = argv[i];
	       continue;

	   case 't':
	       if (++i < argc)
		   temp_dir = argv[i];
	       continue;

	  default:
	       goto lusage;
	  }
	  if (trn_file == NULL)
	       trn_file = argv[i];
	  else if (location == NULL)
	       location = argv[i];
	  else goto lusage;
     }

     if (trn_file == NULL || location == NULL)
	  goto lusage;					/* required */

     if ((trnf = open (trn_file, O_RDONLY, 0)) < 0) {
	  fprintf (stderr, "Can't open transaction file %s\n", trn_file);
	  exit(1);
     }

     trnfsize = fsize (trnf);
     read_header();				/* check meeting */


     /* search for last transaction, which gives us our best toehold into
	the meeting.  This is done by searching from the end of the meeting
	for unique. */
     read_last_trn();				/* read last_trn into th */

     if (th.current > MAX_TRNS) {
	  fprintf (stderr, "More transactions than expected: %d\n", th.current);
	  exit(1);
     }

     /* zero out array */
     num_trns = th.current;
     for (i = 0; i <= num_trns; i++) {
	  trn_pos[i] = 0;
     }

     /* loop to fill out transaction position array */
     while (current_pos != 0) {
	  read_trn_hdr (current_pos);
	  trn_pos [th.current] = current_pos;

	  current_pos = th.prev_trn;
     }

     create_temp();

     for (i = 1; i <= num_trns; i++) {
	  if (trn_pos [i] != 0)
	       save_trn (trn_pos [i]);
	  else {
	       int result;

	       expunge_trn(location, i, &result);
	       if (result != 0) {
		    fprintf(stderr, "Error expunging transaction [%04d]: %s\n", i, error_message(result));
		    exit(1);
	       }
	  }
     }

     exit (0);

lusage:
     fprintf(stderr, "usage: recover trn_file mtg_name {-c chairman} {-n name}\n");
     exit (1);
}

read_header()
{
     int result;
     int pos;

     lseek(trnf,0,0);				/* rewind file */
     if (read (trnf, &tb, sizeof (trn_base)) < sizeof (trn_base)) {
	  fprintf (stderr, "Can't read trn_base\n");
	  exit(1);
     }

     if (tb.unique != TRN_BASE_UNIQUE) {
	     /*
	      * Try byte swapping the arguments before giving up.
	      */
	     S(tb.unique);
	     if (tb.unique == TRN_BASE_UNIQUE)
		     do_byteswap++;
	     else {
		     fprintf (stderr, "Invalid trn_base unique\n");
		     exit(1);
	     }
     }

     if (do_byteswap) {
	     S(tb.version);
	     S(tb.date_created);
	     S(tb.long_name_addr);
	     S(tb.chairman_addr);
	     S(tb.long_name_len);
	     S(tb.chairman_len);
	     S(tb.public_flag);
     }
	     
     if (tb.version != TRN_BASE_1) {
	  fprintf (stderr, "Invalid trn_base version\n");
	  exit(1);
     }

     /* read the chairman */
     if (chairman == NULL) {
	  if (tb.chairman_len > 255) {
	       fprintf (stderr, "Unreasonable chairman length: %d\n",
			tb.chairman_len);
	       exit(1);
	  }
	  chairman = malloc (tb.chairman_len);
	  if (lseek(trnf, tb.chairman_addr, 0) < 0) {
no_chairman:
	       fprintf (stderr, "Can't read chairman\n");
	       exit(1);
	  }
	  
	  if (read (trnf, chairman, tb.chairman_len) < tb.chairman_len)
	       goto no_chairman;
     }
	  
     /* read the long name */
     if (mtg_name == NULL) {
	  if (tb.long_name_len > 255) {
	       fprintf (stderr, "Unreasonable long_name length\n");
	       exit(1);
	  }
	  mtg_name = malloc (tb.long_name_len);
	  if (lseek(trnf, tb.long_name_addr, 0) < 0) {
no_long_name:
	       fprintf (stderr, "Can't read long_name\n");
	       exit(1);
	  }
	  
	  if (read (trnf, mtg_name, tb.long_name_len) < tb.long_name_len)
	       goto no_long_name;
     }

     strcpy (rpc_caller, chairman);
     /* got the params, now create the meeting */
     create_mtg_priv (location, mtg_name, tb.public_flag, tb.date_created, chairman, NULL, &result);
     if (result != 0) {
	  fprintf (stderr, "Couldn't create meeting, %s", error_message(result));
	  exit(1);
     }

     pos = sizeof (tb) + tb.long_name_len + tb.chairman_len;
     if (pos == trnfsize) {
	  fprintf (stderr, "Empty meeting\n");
	  exit (0);
     }
}

/*
 *
 * read_trn_hdr () -- Procedure to read a transaction header, erroring out
 *		      if not kosher.
 *
 */
static
read_trn_hdr (position)
int position;
{
     if (lseek (trnf, position, 0) < 0) {
no_read:
	  fprintf (stderr, "Can't find transaction at %d\n", position);
	  exit (1);
     }

     if (read (trnf, &th, sizeof (th)) != sizeof (th))
	  goto no_read;

     if (do_byteswap) {
	     S(th.version);
	     S(th.unique);
	     S(th.current);
	     S(th.orig_pref);
	     S(th.date_entered);
	     S(th.num_lines);
	     S(th.num_chars);
	     S(th.prev_trn);
	     S(th.subject_addr);
	     S(th.author_addr);
	     S(th.text_addr);
	     S(th.subject_len);
	     S(th.author_len);
     }
     
     /* safety checks */
     if (th.version != TRN_HDR_1) {
	  fprintf (stderr, "Invalid trn_hdr version at %d\n", position);
	  exit(1);
     }

     if (th.unique != TRN_HDR_UNIQUE) {
	  fprintf (stderr, "Invalid trn_hdr unique\n");
	  exit(1);
     }

     if (th.current <= 0 || th.author_len < 0 || th.subject_len < 0 || th.num_chars < 0 || th.num_lines < 0) {
	  fprintf (stderr, "Negative number\n");
	  exit(1);
     }

     if (th.author_addr+th.author_len > trnfsize || th.subject_addr+th.subject_len > trnfsize || th.text_addr+th.num_chars > trnfsize) {
	  fprintf (stderr, "Field past file\n");
	  exit(1);
     }

     return;
}
/*
 *
 * maybe_read_trn_hdr -- procedure to try to read a transaction header,
 *			 returning true if successful.
 *
 */
static
maybe_read_trn_hdr(position)
int position;
{
     if (lseek (trnf, position, 0) < 0) {
	  return (FALSE);
     }

     if (read (trnf, &th, sizeof (th)) != sizeof (th))
	  return (FALSE);

     if (do_byteswap) {
	     S(th.version);
	     S(th.unique);
	     S(th.current);
	     S(th.orig_pref);
	     S(th.date_entered);
	     S(th.num_lines);
	     S(th.num_chars);
	     S(th.prev_trn);
	     S(th.subject_addr);
	     S(th.author_addr);
	     S(th.text_addr);
	     S(th.subject_len);
	     S(th.author_len);
     }	     
     
     /* safety checks */
     if (th.version != TRN_HDR_1) {
	  return (FALSE);
     }

     if (th.unique != TRN_HDR_UNIQUE) {
	  return (FALSE);
     }

     if (th.current <= 0 || th.author_len < 0 || th.subject_len < 0 || th.num_chars < 0 || th.num_lines < 0) {
	  return (FALSE);
     }

     if (th.author_addr+th.author_len > trnfsize || th.subject_addr+th.subject_len > trnfsize || th.text_addr+th.num_chars > trnfsize) {
	  return (FALSE);
     }

     return (TRUE);
}

/*
 *
 * read_last_trn -- brute force routine to just try reading transactions until
 *		    get a success.
 *
 */
static
read_last_trn ()
{
     current_pos = trnfsize - sizeof (th) - 2;

     while (current_pos > sizeof (tb)) {
	  if (maybe_read_trn_hdr (current_pos))
	       return;
	  current_pos--;
     }
}

static
save_trn (position)
int position;
{
     char *th_subject, *th_author, *th_signature;
     tfile tf;
     int tfs,tocopy;
     trn_nums result_trn;
     int result;
     char buffer[512];

     read_trn_hdr (position);

     th_subject = malloc (th.subject_len);
     lseek (trnf, th.subject_addr, 0);
     read (trnf, th_subject, th.subject_len);
	  
     th_author = malloc (th.author_len);
     lseek (trnf, th.author_addr, 0);
     read (trnf, th_author, th.author_len);

     th_signature = NULL;
     if (strlen (th_author) + 1 != th.author_len) {
	  th_signature = th_author + strlen(th_author) + 1;
     }

     /* start temp file */
     ftruncate(tempf,0);
     lseek(tempf,0,0);

     lseek(trnf, th.text_addr, 0);
     tfs = th.num_chars;
     while (tfs > 0) {
	  tocopy = min (512, tfs);
	  read (trnf, buffer, tocopy);
	  write (tempf, buffer, tocopy);
	  tfs -= tocopy;
     }

     lseek(tempf,0,0);

     tf = unix_tfile (tempf);

     add_trn_priv (location, tf, th_subject, th_signature, th.orig_pref, th.current, th_author, th.date_entered, 0, &result_trn, &result);
     if (result != 0) {
	  fprintf (stderr, "Couldn't add transaction %d; %s", th.current, error_message(result));
	  exit(1);
     }

     free(th_author);
     free(th_subject);
     tdestroy (tf);
     printf ("Added transaction %d\n", th.current);
     return;
}

/*
 *
 * fsize () -- Routine to find out the size of a file.
 *
 */
static
fsize (d)
int d;
{
     struct stat buf;

     if (fstat (d, &buf) < 0)
	  return (0);

     return (buf.st_size);
}

/*
 *
 * new_string (s)  --   Routine to create a copy of the given string, using
 *		      	malloc.
 *
 */
static
char *new_string (s)
char *s;
{
     int len;
     char *newstr;

     len = strlen (s) + 1;
     newstr = malloc (len);
     strcpy (newstr, s);
     return (newstr);
}

/*
 *
 * create_temp () -- Create temp file, and let it be tempf.
 *
 */
create_temp()
{
     char *filename;

     filename = malloc (strlen (temp_dir) + 10);
     strcpy (filename, temp_dir);
     strcat (filename, "/rcXXXXXX");
     mktemp (filename);

     tempf = open (filename, O_RDWR | O_CREAT, 0700);
     if (tempf < 0) {
	  fprintf (stderr, "Cannot open temp file `%s'\n", filename);
	  exit (1);
     }
}
