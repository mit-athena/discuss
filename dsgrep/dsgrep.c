/*
 *
 *      Copyright (C) 1991 by the Massachusetts Institute of Technology
 *      Developed by the MIT Student Information Processing Board (SIPB).
 *      For copying information, see the file mit-copyright.h in this release.
 *
 */

#ifndef lint
#ifndef SABER
static char *RCSid = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/dsgrep/dsgrep.c,v 1.6 1996-09-19 22:29:00 ghudson Exp $";
#endif
#endif

#include "regexp.h"

#define MAX_MEETINGS 128

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/file.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <discuss/discuss.h>
#ifdef SOLARIS
#include <macros.h>
#define MAX max
#endif

extern tfile unix_tfile();
extern tfile mem_tfile();
extern char *malloc(),*calloc();

extern int errno;
extern char *sys_errlist[];
extern int sys_nerr;

int verbose_errors;
int bsize;
char *buffer;

main(argc,argv)
     int argc;
     char *argv[];
{
  extern int optind;
  extern char *optarg;

  char *meetings_file,*homedir,*getenv();
  name_blk *meetings,*tmp_mtg;
  mtg_info meeting_info[MAX_MEETINGS];
  char tmp_meeting_file[MAXPATHLEN];
  int n_meetings,result,setenv(),i,cur_meeting,n_to_look,j,c;
  int print_trans,use_re,search_trans,search_deleted,s_trans();
  int case_insens,trans_num;
  int high,low;
  regexp *search_re;
  tfile tf;
  trn_info2 ti;
  void s_to_lower();
  int tmp_fd1,tmp_fd2;
  int using_dflt_mtgs = 0;
  char *tmp_buf;
  struct stat statb;
  
  initialize_dsc_error_table();

  n_to_look = 50;
  print_trans = 0;
  search_trans = 0;
  use_re = 0;
  verbose_errors = 0;
  case_insens = 0;
  search_deleted = 0;
  trans_num = 0;
  meetings_file = NULL;

  while ((c = getopt(argc,argv, "n:e:f:t:apvdhi")) != EOF)
    switch(c) {
    case 'n':
      n_to_look = atoi(optarg);
      break;
    case 't':
      trans_num = atoi(optarg);
      break;
    case 'p':
      print_trans=1;
      break;
    case 'd':
      search_deleted = 1;
      break;
    case 'a':
      search_trans=1;
      bsize = 4096;
      if ((buffer = malloc((unsigned) bsize)) == NULL) {
        fprintf(stderr,"dsgrep: malloc failed\n");
	exit(1);
      }
      break;
    case 'e':
      if ((search_re = regcomp(optarg)) == NULL) {
	fprintf(stderr,"dsgrep: Invalid regular expression %s\n",optarg);
	exit(1);
      }
      use_re = 1;
      break;
    case 'f':
      meetings_file = optarg;
      break;
    case 'v':
      verbose_errors=1;
      break;
    case 'i':
      case_insens=1;
      break;
    case '?':
    case 'h':
      fprintf(stderr,"usage: dsgrep [-n n_trans]\n");
      fprintf(stderr,"           [-e title_regexp]\n");
      fprintf(stderr,"           [-t trans_num] print out a specific trans\n");
      fprintf(stderr,"           [-a]  (search text as well as title)\n");
      fprintf(stderr,"           [-p]  (print matching transactions)\n");
      fprintf(stderr,"           [-v]  (print out verbose error messages)\n");
      fprintf(stderr,"           [-d]  (search deleted transactions as well)\n");
      fprintf(stderr,"           [-i]  (convert text to lower case before searching)\n");
      fprintf(stderr,"           [-f alt_meeting_file]\n");
      fprintf(stderr,"           [meetings]   (this must be last)\n");
      exit(1);
      break;
    }

  if (meetings_file == NULL) {  /* Nothing set, use out of home directory */
    if ((homedir = getenv("HOME")) == NULL) {
      fprintf(stderr,"dsgrep: could not get HOME environment variable\n");
      exit(1);
    }
    meetings_file = (char *)malloc(256);
    strcpy(meetings_file,homedir);
    strcat(meetings_file,"/.meetings");
  }
  if (setenv("MEETINGS",meetings_file,1) == -1) {
    fprintf(stderr,"dsgrep: could not add environment variable\n");
    exit(1);
  }

  switch (optind - argc) {
  case 0:
    dsc_expand_mtg_set(NULL,"*",&meetings,&cur_meeting,&result);
    break;
  case 1:
    dsc_expand_mtg_set(NULL,argv[optind],&meetings,&cur_meeting,&result);
    break;
  default:
    meetings = (name_blk *)calloc((unsigned)MAX_MEETINGS,sizeof(name_blk)); 
    cur_meeting = 0;
    for(i=optind;i<argc;i++) {
      dsc_expand_mtg_set(NULL,argv[i],&tmp_mtg,&n_meetings,&result);
      if ((n_meetings == 0) && (result == 0)) {
	fprintf(stderr,"dsgrep: no such meeting %s in %s\n",argv[i],
		meetings_file);
	continue;
      }
      if (result != 0) {
	fprintf(stderr,"dsgrep: error expanding meeting %s: %s\n",
		argv[i],error_message(result));
	exit(1);
      }
      memcpy(&meetings[cur_meeting],tmp_mtg,sizeof(name_blk));
      cur_meeting++;
    }
  }

  if (cur_meeting == 0) {
    fprintf(stderr,"No meetings selected.\n");
    exit(1);
  }

  tf = unix_tfile(1); /*stdout */

  for(i=0;i<cur_meeting;i++) {
    dsc_get_mtg_info(&meetings[i],&meeting_info[i],&result);
     if (result != 0) {
       fprintf(stderr,"dsgrep: error getting meeting info for meeting '%s:%s': %s\n",
	       meetings[i].hostname, meetings[i].pathname,
	       error_message(result));
       continue;
     }
    if (trans_num != 0) {
      low = trans_num;
      high = trans_num;
    } else {
      low = meeting_info[i].highest-n_to_look+1; 
      low = MAX(low,1);
      high = meeting_info[i].highest;
    }
    for(j=low;j<=high;j++)
      {
	dsc_get_trn_info2(&meetings[i],j,&ti,&result);
	if (result != 0) {
	  if (verbose_errors)
	    fprintf(stderr,"dsgrep: error getting transaction info for %s[%d]: %s\n",
		    strrchr(meetings[i].pathname,'/')+1, j,
		    error_message(result));
	  continue;
	}
	if (!search_deleted && (ti.flags & TRN_FDELETED))
	  continue;
	if (case_insens) s_to_lower(ti.subject);
	if (!use_re || regexec(search_re,ti.subject) ||
	    (search_trans &&
	     s_trans(meetings[i],j,ti.num_chars,search_re,case_insens)))  {
	  printf("%s [%d]: %s\n",
		 strrchr(meetings[i].pathname,'/')+1, j, ti.subject);
	  if (print_trans) {
	    dsc_get_trn(&meetings[i],j,tf,&result);
	    if ((result != 0) && verbose_errors)
	      fprintf(stderr,"dsgrep: error getting transaction %s[%d]: %s\n",
		      strrchr(meetings[i].pathname,'/')+1, j,
		      error_message(result));
	    printf("*** End of Transaction ***\n");
	  }
	}
      }
  }
  if (using_dflt_mtgs)
    unlink(tmp_meeting_file);
  exit(0);
}

int
s_trans(nbp,trans_no,n_chars,search_re,case_insens)
     name_blk nbp;
     int trans_no,n_chars;
     regexp *search_re;
     int case_insens;
{
  tfile tf;
  int result;
  void s_to_lower();

  if (++n_chars > bsize) {
    free(buffer);
    if ((buffer = malloc((unsigned)n_chars)) == NULL) {
      fprintf(stderr,"dsgrep: malloc failed\n");
      exit(1);
    }
    bsize = n_chars;
  }
  tf = mem_tfile(buffer,bsize);
  dsc_get_trn(&nbp,trans_no,tf,&result);
  if ((result != 0) && verbose_errors){
    fprintf(stderr,"dsgrep: error getting transation %s[%d]: %s\n",
	    strrchr(nbp.pathname,'/')+1,trans_no,
	    error_message(result));
  }
  tdestroy(tf);
  buffer[n_chars-1] = '\0';
  if (case_insens) s_to_lower(buffer);
  return(regexec(search_re,buffer));
}

void
s_to_lower(bufp)
     char *bufp;
{
  while (*bufp != '\0') {
    if (isupper(*bufp)) *bufp = tolower(*bufp);
    bufp++;
    }
}
