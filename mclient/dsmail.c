 /*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * dsmail.c - Modifies/parses mail to discuss
 *
 *        $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/dsmail.c,v $
 *
 *        $Log: not supported by cvs2svn $
 * Revision 1.1  94/04/08  12:37:12  vrt
 * Initial revision
 * 
 * Revision 1.23  93/07/14  02:14:35  srz
 * Check for DELETED_TRN on reply transaction.
 * 
 * Revision 1.22  1993/05/19  15:36:13  raeburn
 * Handle errors in setting up temp file properly.
 * Also, make RPC_NS_TIMEOUT a EX_TEMPFAIL ("try again later") error.
 *
 * Revision 1.21  1993/02/16  21:10:13  srz
 * Trim trailing white-space when comparing subjects.  If daemon does not have
 * access to read long name of meeting, use in-reply-to transaction number
 * regardless of meeting name.
 *
 * Revision 1.20  1993/02/09  23:47:04  srz
 * Fix continuation lines in header fields.  Add better In-reply-to: handling to
 * deal with "[xxxx] in meeting_name" syntax, not replying if meeting name does
 * not match.  Add -s option for subject matching.
 *
 * Revision 1.19  1992/09/11  19:45:36  srz
 * Enter transaction anyways if In-reply-to: transaction does not exist.
 *
 * Revision 1.18  1992/06/26  02:20:05  raeburn
 * getting in sync with current source tree
 *
 * Revision 1.17  92/01/10  00:03:58  srz
 * Haynes' fix for realloc stuff (on Suns).
 * 
 * Revision 1.16  91/07/27  02:27:28  srz
 * Added check in case EX_CONFIG wasn't defined.
 * 
 * Revision 1.15  91/07/05  20:48:09  bjaspan
 * ported to svr4
 * 
 * Revision 1.14  91/07/05  19:16:33  bjaspan
 * actually, I meant ucbinclude
 * 
 * Revision 1.13  91/07/05  19:12:47  bjaspan
 * use /usr/ucb/sysexits.h on svr4
 * 
 * Revision 1.12  91/05/29  16:54:27  raeburn
 * Discarded some unused code; use exit codes that sendmail will recognize, and
 * use SMTP codes in error messages printed.  Renamed PipeToMeeting to
 * SendToMeeting.  Some other minor changes dealing with initialization of
 * variables.  Deleted useless `p' option, added h to indicate that hostname
 * should be extracted from meeting name (not currently used, though).
 * 
 * Revision 1.11  90/02/24  18:46:12  srz
 * Added deriving signature from From: field.
 * 
 * Revision 1.10  89/06/03  00:30:54  srz
 * Added standard copyright notice.
 * 
 * Revision 1.9  89/04/17  17:09:21  srz
 * Added error table initialization.
 * 
 * Revision 1.8  89/01/05  07:13:11  raeburn
 * replaced included header files with <discuss/discuss.h>; also added
 * newline after message header
 * 
 * Revision 1.7  88/01/15  22:42:39  srz
 * set_module now returns fatal flag, again.
 * 
 * Revision 1.6  88/01/05  03:09:30  srz
 * Put location of DSPIPE into config.h
 * 
 * Revision 1.5  87/10/24  04:26:44  wesommer
 * Rewritten for speed and efficiency.  "popen" is a crock.
 * 
 * Revision 1.4  87/07/17  03:34:57  tytso
 * Bugfixes made, added -p option, removed zippy stuff
 * 
 * Revision 1.3  87/05/02  02:12:58  tytso
 * Replaced the regexp and options parsing code with the UNIX
 * library routines.  Cleaned up code.  Added -A (all headers)
 * option.
 * 
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/file.h>
#if defined(SVR4) || defined (SYSV)
#include "/usr/ucbinclude/sysexits.h"
#define EX_CONFIG EX_SOFTWARE
#else
#include <sysexits.h>
#endif

#include <discuss/discuss.h>
#include <rpc.h>
#include "config.h"

#define DEFAULT_SUBJECT "No subject found in mail header"
#define BUFFLEN 400
#define LISTLEN 40

/* Ultrix sux */
#ifndef EX_CONFIG
#define EX_CONFIG EX_SOFTWARE
#endif

#ifndef	lint
static char rcsid[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/dsmail.c,v 1.2 1994-08-14 22:21:28 cfields Exp $";
#endif

char *malloc(), *realloc ();
char *mktemp();
FILE *popen();
FILE *fopen();
FILE *fdopen();
int getopt();

extern char *optarg;                /* External variables for getopt */
extern int optind;
extern int regerrno;

char *deflist[] = {
	"^to$","^from$","^cc$",".*-to$",".*-from$","^date$", NULL
};
		
char *subjlist[] = {
	"subject",NULL
};

char *inreplyto[] = {
	"in-reply-to",NULL
};

char *fromlist[] = {
	"^from$",NULL
};     

char *progname;
char *save[LISTLEN],*reject[LISTLEN];
char *usr_mtg = "";
int dodefs, allfields, debug, have_host, subject_match=0;
char *optarg;
int optind;
extern tfile unix_tfile();
int reply_to;

void PRS();
void lower();
void trim();
void SendToMeeting();
int parse_reply_to();
int have_trn_num();
int subject_compare();

extern char *malloc(),*realloc();

main(argc,argv)
	int argc;
	char *argv[];
{
     FILE *f;
     char line[BUFFLEN+1],*colonp,*ep;
     char *subject=NULL,*reply_to_str=NULL;
     int i,ok_prev=0,iscont = 0, have_signature = 0, len;
     char filename[60], signature[35], field_name[100];
     
     init_dsc_err_tbl();

     PRS(argc,argv);				/* Parse args */

     line[BUFFLEN]='\0';

     if (debug) 
	  f=stdout;
     else {
	  (void) mktemp(strcpy(filename, "/tmp/DSXXXXXX"));
	  if ((f = fopen(filename,"w+")) == NULL)
	    {
	      fprintf (stderr, "250-Can't create temporary file ");
	      fflush (stderr);
	      perror(filename);
	      return EX_TEMPFAIL;
	    }
	  if (fchmod(fileno(f),0600))
	    {
	      perror("250-Can't chmod temporary file");
	      return EX_TEMPFAIL;
	    }
     }

     if (debug) printf("start of text->");
     
     if (fgets(line,BUFFLEN,stdin)==NULL)
	  exit(EX_NOINPUT);

     while (*line != '\n') {			/* Do headers */
	  if (!iscont) {
	       char *cp;

	       ep = line + strlen(line);
	       colonp = strchr(line, ':');
	       if (colonp == NULL)
		    colonp = ep;
	       len = colonp - line;
	       if (len > sizeof(field_name))
		    len = sizeof(field_name)-1;
	       strncpy(field_name, line, len);
	       field_name[len] = '\0';
	       lower(field_name);

	       if (list_compare(field_name,subjlist)) {	/* Subject */
		    cp = colonp;
		    if (*cp == ':')
			 cp++;
		    while (isspace(*cp))
			 cp++;

		    /* ignore second subject line */
		    if (subject==NULL) {
			 subject=malloc(ep-cp+1);
			 (void) strcpy(subject,cp);

			 /* Trim subject */
			 trim(subject);
		    }	
	       } else if (list_compare(field_name, inreplyto)) {
		    /* ignore second subject line */
		    cp = colonp;
		    if (*cp == ':')
			 cp++;

		    if (reply_to_str==NULL) {
			 reply_to_str=malloc(ep-cp+1);
			 (void) strcpy(reply_to_str,cp);
		    }	
	       }
	       else if (list_compare(field_name,fromlist)) {
		    cp = colonp;
		    if (*cp == ':') {
			 cp++;
			 extract_full_name(cp, signature, sizeof(signature));
			 have_signature = TRUE;
			 if (debug)
			      printf ("got sig: %s\n",
				      signature);
		    }
	       }
	  } else {
	       if (list_compare(field_name, inreplyto) && reply_to_str != NULL) {
		    /* Concat reply-to */
		    reply_to_str = realloc(reply_to_str,strlen(reply_to_str) + strlen(line) + 1);
		    strcpy(reply_to_str+strlen(reply_to_str),line);
	       }
	  }

	  if ((ok_prev && iscont) || (!iscont && CheckField(field_name))) {
	       ok_prev=1;
	       fprintf(f,"%s",line);
	  } else
	       ok_prev=0;

	  iscont = line[strlen(line)-1] != '\n';
	  if (fgets(line,BUFFLEN,stdin)==NULL)
	       goto bye;
	  if (!iscont)
	       iscont = (line[0] == ' ' || line[0] == '\t');
     }
     fprintf (f, "\n");

     /* Copy over body of message */
     while (fgets(line,BUFFLEN,stdin)!=NULL) {
	  fprintf(f,"%s",line);
     }
     if (fflush(f) == EOF) {
	  fprintf (stderr, "250-Can't write to ");
	  fflush (stderr);
	  perror (debug ? "standard output" : filename);
	  exit (EX_TEMPFAIL);
     }
     
     SendToMeeting(f,(subject==NULL) ? DEFAULT_SUBJECT : subject,
		   reply_to_str,
		   (have_signature) ? signature : NULL);
 bye:
     if (!debug)
	  (void) unlink(filename); 
     exit (EX_OK);
     
}

void SendToMeeting(f, subject, reply_to_str, signature)
char *subject;
char *reply_to_str;
char *signature;     
FILE *f;
{
     int len,have_mtg_info;
     static char module[100] = "discuss@";
     char *mtg_name,*cp,*short_name;
     int fatal_err, result, smtp_code, exit_code, reply_to_trn;
     tfile transaction;
     mtg_info minfo;
     trn_info tinfo;
     int trn_no, i;
     
     gethostname(&module[8], sizeof(module)-8);
     init_rpc();
     set_module(module, &fatal_err, &result);

     switch (result) {
     case 0:
	  break;
     case RPC_NS_TIMEOUT:
	  smtp_code = 250;
	  exit_code = EX_TEMPFAIL;
	  break;
     default:
	  smtp_code = 554;
	  exit_code = EX_CONFIG;
	  break;
     }
     if (result) {
	  fprintf(stderr, "%03d-Can't connect to discuss server: %s\n",
		  smtp_code, error_message(result));
	  exit (exit_code);
     }
     
     rewind(f); lseek(fileno(f), 0, L_SET);
     transaction = unix_tfile(fileno(f));

     cp = strchr(subject, '\n');
     if (cp)
	  *cp = '\0';

     /* Parse reply_to_string */
     have_mtg_info = FALSE;
     reply_to_trn = 0;
     if (reply_to_str != NULL && have_trn_num(reply_to_str)) {
	  short_name = usr_mtg + strlen(usr_mtg);
	  while (short_name > usr_mtg && *short_name != '/')
	       short_name--;

	  if (*short_name == '/')
	       short_name++;

	  reply_to_trn = parse_reply_to(reply_to_str, short_name);
	  if (reply_to_trn == 0) {
	       /* Get mtg info, to check out long name */
	       get_mtg_info(usr_mtg, &minfo, &result);
	       if (result == 0) {
		    have_mtg_info = TRUE;
		    reply_to_trn = parse_reply_to(reply_to_str, minfo.long_name);
	       } else
		    reply_to_trn = parse_reply_to(reply_to_str, NULL);
	  }
     }

     if (reply_to_trn == 0 && subject_match && strcmp(subject, DEFAULT_SUBJECT) && *subject != '\0') {
	  if (!have_mtg_info) {
	       /* Get mtg info, to check out long name */
	       get_mtg_info(usr_mtg, &minfo, &result);
	       if (result == 0)
		    have_mtg_info = TRUE;
	  }
	  if (have_mtg_info) {
	       /* Walk through meeting, doing subject comparisons */
	       for (i = 0; i < subject_match; i++) {
		    if (minfo.last-i < minfo.first)
			 break;

		    get_trn_info(usr_mtg, minfo.last-i, &tinfo, &result);
		    if (result == 0) {
			 if (subject_compare(tinfo.subject, subject)) {
			      reply_to_trn = minfo.last-i;
			      dsc_destroy_trn_info(&tinfo);
			      break;
			 }
			 dsc_destroy_trn_info(&tinfo);
		    }
	       }
	  }
     }

     if (debug) {
	  printf("subject is ``%s''\nreply to %d in %s\n",
		 subject, reply_to_trn, usr_mtg);
     } 

     if (signature == NULL || *signature == '\0') {
	  add_trn(usr_mtg, transaction, subject, reply_to_trn, &trn_no, &result);
     } else {
	  if (get_server_version() < SERVER_2) {
	       add_trn(usr_mtg, transaction, subject, reply_to_trn, &trn_no, &result);
	  } else {
	       add_trn2(usr_mtg, transaction, subject, signature, reply_to_trn, &trn_no, &result);
	  }
     }

     /* If error, try it as a non-reply */
     if ((result == NO_SUCH_TRN || result == DELETED_TRN || result == NO_ACCESS)  && reply_to_trn != 0) {
	  tdestroy(transaction);
	  rewind(f); lseek(fileno(f), 0, L_SET);
	  transaction = unix_tfile(fileno(f));
	  if (signature == NULL || *signature == '\0' || get_server_version() < SERVER_2) {
	       add_trn(usr_mtg, transaction, subject, 0, &trn_no, &result);
	  } else {
	       add_trn2(usr_mtg, transaction, subject, signature, 0, &trn_no, &result);
	  }
     }
     
     switch (result) {
     case 0:
	  break;
     default:
	  smtp_code = 550;
	  exit_code = EX_CANTCREAT;
     }
     if (result) {
	  fprintf (stderr, "%03d-Can't enter transaction", smtp_code);
	  if (reply_to)
	       fprintf (stderr, " as reply to %d", reply_to);
	  fprintf (stderr, "\n%03d- into meeting %s: %s\n",
		   smtp_code, usr_mtg, error_message (result));
	  exit(exit_code);
     }
     (void) fclose(f);
}

void
lower(s)
char *s;
{
     while (*s) {
	  if (isupper(*s))
	       *s = tolower(*s);
	  s++;
     }
}

void
trim(s)
char *s;
{
     char *cp;

     cp = &s[strlen(s)]-1;
     while (cp >= s && isspace(*cp)) {
	  *cp-- = '\0';
     }
}

int CheckField(key)
	char *key;
{
	int keepfield;

	keepfield=allfields;
	if (!keepfield && dodefs && list_compare(key,deflist))
		keepfield=1;
	if (!keepfield && list_compare(key,save))
		keepfield=1;
	if (keepfield && list_compare(key,reject))
		keepfield=0;
	return(keepfield);
}

/* Parse command line arguments */
void PRS(argc,argv)
	int argc;
	char **argv;
{
	int c,rp,sp;
	
	progname=argv[0];
	sp=rp=0;
	optind=1;		/* Initialize for getopt */
	while ((c = getopt(argc,argv,"AZDs:da:r:h")) != EOF)
		switch(c) {
		case 'd':
			dodefs=!dodefs;
			break;

		case 's':
			subject_match = atoi(optarg);
			break;

		case 'D':
			debug=!debug;
			break;

		case 'A':
			allfields=!allfields;
			break;

		case 'a':
			lower(optarg);
			save[sp++]=optarg;
			if (sp>=LISTLEN) {
				fprintf(stderr,"500-Too many accept fields\n");
				exit (EX_USAGE);
			}
			break;

		case 'h':
			have_host = 1;
			break;

		case 'r':
			lower(optarg);
			reject[rp++]=optarg;
			if (sp>=LISTLEN) {
				fprintf(stderr,"500-Too many reject fields\n");
				exit (EX_USAGE);
			}
		}    
	if (optind>=argc) 
		goto lusage;
	usr_mtg=argv[optind];
	if (have_host) {
		usr_mtg = strchr (usr_mtg, ':');
		if (!usr_mtg)
			goto lusage;
		usr_mtg++;
	}
	save[sp]=NULL;		/* Insert terminators */
	reject[rp]=NULL;
	return;
 lusage:
	printf("500-Usage: %s [-dADZ] [-s subject-match-count] [-a field] [-r field] meeting-path-name\n",
	       progname);
	exit (EX_USAGE);
}

char *re_comp();

int list_compare(s,list)
	char *s,**list;
{
	char *err;

	while (*list!=NULL) {
		err=re_comp(*list++);
		if (err) {
			fprintf(stderr,"554-%s - %s\n",
				progname, err, list[-1]);
			exit(EX_CONFIG);
			}
		if (re_exec(s))
			return(1);
	}
	return(0);
}

strip_addr(addr, dest, dest_size)
char *addr,*dest;
int dest_size;
{
     char *dest_end,*dp,*sp,*quote_start;
     int paren_level,found_angle;

     dest_end = &dest[dest_size-1];
     dp = dest;
     sp = addr;
     paren_level = 0;

eat_white:
     while (isspace(*sp) && *sp != '\n')
	  sp++;

     if (*sp == '(')
	  goto eat_comment;

     if (*sp == '"')
	  goto eat_string;

     if (*sp == '<') {
	  dp = dest;
	  sp++;
	  found_angle = TRUE;
	  goto eat_white;
     }

     if (*sp == '>' && found_angle) {
	  *sp++;
	  goto eat_white;
     }

     if (*sp == '\0' || *sp == '\n') {
	  *dp++ = '\0';
	  goto post_proc;
     }

     *dp++ = *sp++;
     if (dp == dest_end) {
	  *dp++ = '\0';
	  goto post_proc;
     }

     goto eat_white;

eat_comment:
     paren_level++;
     sp++;

cont_comment:
     while (*sp != ')' && *sp != '(' && *sp) {
	  sp++;
     }

     if (*sp == '\0') {
	  *dp = '\0';
	  goto post_proc;
     }

     if (*sp == '(')
	  goto eat_comment;

     sp++;		/* ) */
     paren_level--;
     if (paren_level <= 0)
	  goto eat_white;

     goto cont_comment;

eat_string:
     quote_start = sp;
     sp++;

     while(*sp != '"' && *sp) 
	  sp++;

     if (!*sp) {
	  *dp = '\0';
	  goto post_proc;
     }

     if (*++sp == '@') {		/* "foo"@bar */
	  sp = quote_start;
	  *dp++ = *sp++;
	  while (dp < dest_end && *sp != '"')
	       *dp++ = *sp++;

	  if (dp == dest_end) {
	       *dp = '\0';
	       goto post_proc;
	  }
	  *dp++ = *sp++;
	  if (dp == dest_end) {
	       *dp++ = '\0';
	       goto post_proc;
	  }
     }
     goto eat_white;

     /* No post processing */
post_proc:
     return;
}

/*
 *
 *   Routine to extract a full name from an address.  If no full name
 *   can be found, then we simply return the stripped address.
 *
 */
extract_full_name(addr, dest, dest_size)
char *addr,*dest;
int dest_size;
{
     char *dest_end,*dp,*sp,*bracket,*close_paren;
     int paren_level,non_white;

     dest_end = &dest[dest_size-1];
     dp = dest;
     sp = addr;

     /* Find angle bracket (if possible) */
     while (*sp && *sp != '<' && *sp != '\n')
	  sp++;

     bracket = NULL;
     if (*sp == '<')
	  bracket = sp;

     non_white = 0;
     if (bracket != NULL) {
	  for (sp = addr; sp < bracket; sp++) {
	       if (!isspace(*sp) && *sp != '"')
		    non_white++;
	  }
     }

     if (non_white > 1) {		/* We have a name */
	  sp = addr;
	  while (isspace(*sp) || *sp == '"')	/* Skip leading spaces */
	       sp++;

	  while (isspace(*(bracket-1)) || *(bracket-1) == '"')  /* Skip trailing spaces */
	       bracket--;

	  /* Copy it over */
	  while (sp < bracket && dp < dest_end)
	       *dp++ = *sp++;

	  *dp++ = '\0';
	  return;
     }

     /* Now, let's see if we have name in a comment (look back from the
	end for a parenthesis. */
     for (sp = addr; *sp && *sp != '\n'; sp++)
	  ;

     sp--;
     while (sp > addr && isspace(*sp))
	  sp--;

     if (*sp == ')') {			/* Name in comment */
	  close_paren = sp;
	  paren_level = 1;
	  sp--;

	  for (;sp > addr; sp--) {
	       if (*sp == ')')
		    paren_level++;
	       else if (*sp == '(') {
		    paren_level--;
		    if (paren_level == 0)
			 break;
	       }
	  }

	  if (*sp == '(') {	      	/* Copy it over */
	       sp++;

	       while(isspace(*sp))
		    sp++;

	       while (sp < close_paren && dp < dest_end)
		    *dp++ = *sp++;

	       *dp = '\0';
	       return;
	  }
     }

     strip_addr(addr, dest, dest_size);
     return;
}	  

/*
 *
 *   parse_reply_to() - Parse an in-reply-to message for a given message.
 *
 */
int
parse_reply_to(str, mtg_name)
char *str;
char *mtg_name;
{
     char *bracketp,*end_bracketp,*startp,*cp;
     int trn_num,mtg_name_len;

     startp = str;
     while (1) {
	  bracketp = strchr(startp, '[');
	  if (bracketp == NULL)
	       return(0);

	  end_bracketp = strchr(bracketp, ']');
	  if (end_bracketp == NULL || end_bracketp - bracketp > 10)
	       return(0);

	  trn_num = 0;
	  cp = bracketp+1;
	  while (isdigit(*cp)) {
	       trn_num = trn_num*10 + *cp++ - '0';
	  }
	  
	  if (*cp == '\\')				/* Skip quoting char */
	       cp++;
	  
	  if (*cp != ']')
	       trn_num = 0;
	  
	  if (trn_num == 0)
	       return(0);
	  
	  cp++;
	  while (isspace(*cp) || *cp == '"')
	       cp++;
	  
	  if (mtg_name == NULL)				/* Match anything */
	       return(trn_num);

	  /* Look for "in" */
	  if (*cp != 'i' && *cp != 'I')
	       return(trn_num);
	  
	  cp++;
	  if (*cp != 'n' && *cp != 'N')
	       return(trn_num);

	  cp++;
	  while (isspace(*cp) || *cp == '"')
	       cp++;

	  /* Check for meeting name.  If it doesn't match, we don't have
	     a transaction number */
	  mtg_name_len = strlen(mtg_name);
	  if (mtg_name == NULL || strlen(cp) < mtg_name_len) {
	       startp = cp;
	       continue;
	  }

	  /* Check if match, and ends fine (random punctuation or space) */
	  if (!strncasecmp(cp, mtg_name, mtg_name_len) &&
	      !isalnum(*(cp+mtg_name_len)) && *(cp+mtg_name_len) != '_' &&
	      *(cp+mtg_name_len) != '-')
	       return(trn_num);

	  startp = cp;
     }
}

int
have_trn_num(str)
char *str;
{
     char *bracketp,*end_bracketp,*cp;
     int trn_num;

     bracketp = strchr(str, '[');
     if (bracketp == NULL)
	  return(FALSE);

     end_bracketp = strchr(bracketp, ']');
     if (end_bracketp == NULL || end_bracketp - bracketp > 10)
	  return;

     trn_num = 0;
     cp = bracketp+1;
     while (isdigit(*cp)) {
	  trn_num = trn_num*10 + *cp++ - '0';
     }

     if (*cp == '\\')				/* Skip quoting char */
	  cp++;

     if (*cp != ']')
	  trn_num = 0;

     if (trn_num == 0)
	  return(FALSE);

     return(TRUE);
}

/*
 *
 *   Compare subjects, ignoring 're: *' at the beginning and trailing
 *	white space
 *
 */
int
subject_compare(subj1, subj2)
char *subj1, *subj2;
{
     char *cp;
     int min_len, len1, len2;

     while (*subj1) {
	  while (isspace(*subj1))
	       subj1++;

	  cp = subj1;

	  if (*cp != 'r' && *cp != 'R')
	       break;

	  cp++;
	  if (*cp != 'e' && *cp != 'E')
	       break;
	  cp++;

	  if (*cp != ':')
	       break;

	  subj1 = cp+1;
     }

     while (*subj2) {
	  while (isspace(*subj2))
	       subj2++;

	  cp = subj2;

	  if (*cp != 'r' && *cp != 'R')
	       break;

	  cp++;
	  if (*cp != 'e' && *cp != 'E')
	       break;
	  cp++;

	  if (*cp != ':')
	       break;

	  subj2 = cp+1;
     }

     /* Compare subjects, ignoring trailing white-space */
     len1 = strlen(subj1);
     len2 = strlen(subj2);
     if (len1 < len2)
	  min_len = len1;
     else
	  min_len = len2;

     if (min_len <= 0)
	  return(0);

     if (strncasecmp(subj1, subj2, min_len))
	  return(0);

     if (len1 == len2)
	  return(1);

     if (len1 < len2)
	  cp = &subj2[len1];
     else
	  cp = &subj1[len2];

     /* If rest if white-space, we match */
     while (*cp) {
	  if (*cp != ' ')
	       return(0);
	  cp++;
     }
     return(1);
}
