/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * expunge -- program to expunge a meeting; i.e. really delete those
 *	      deleted transaction.  This program is linked to a server
 *	      so it can use the privileged procedure of create_mtg, and
 *	      the like.
 *
 */

#include <stdio.h>
#ifndef SOLARIS
#include <strings.h>
#else
#include <string.h>
#include <fcntl.h>
#endif
#include <ctype.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#include <discuss/types.h>
#include <discuss/dsc_et.h>
#include <discuss/tfile.h>
#include <discuss/interface.h>
#include <discuss/acl.h>
#include "mtg.h"

#define min(a, b) (a < b ? a : b)

static int tempf;
static char *mtg_name = NULL, *location = NULL, *chairman = NULL, *trn_file = NULL;
static char *backup_location = NULL;
static char *future_location = NULL;
static int found_eof = 0;
static int error_occurred = 0;
static char *temp_dir = "/tmp";
static int daemon_flag = FALSE;

tfile unix_tfile ();
char *malloc();

static lower(), strip_addr(), extract_full_name();
static char *get_header();

extern char rpc_caller[];
extern int has_privs;
extern int errno;
extern int no_nuke, use_zephyr;

main (argc, argv)
int argc;
char **argv;
{
     int i,n,low,high;
     mtg_info old_mtg_info,new_mtg_info;
     trn_info3 old_trn_info;
     int result;
     dsc_acl *acl_list,*new_acl_list;
     dsc_acl_entry *ae;
     char *new_modes, *signature;
     char new_signature[50],dtest[10];
     tfile tf;
     char control_name[256];
     int control_fd;
#ifdef SOLARIS
     static struct flock lock;
#endif


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

	  case 'd':
	       daemon_flag = TRUE;
	       continue;

	  default:
	       goto lusage;
	  }
	  if (location == NULL)
	       location = argv[i];
	  else goto lusage;
     }

     if (location == NULL)
	  goto lusage;					/* required */

     has_privs = TRUE;		/* Tell discuss we're special */
     use_zephyr = 0;		/* Don't notify of every trn copied */
     strcpy (rpc_caller, "expunger");

     /* First, we get the mtg info to make sure it exists */
     get_mtg_info (location, &old_mtg_info, &result);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting mtg info\n", location, error_message(result));
	  exit (1);
     }

     get_acl (location, &result, &acl_list);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting acl\n", location, error_message(result));
	  exit (1);
     }

     /* Create the new meeting */
     backup_location = malloc (strlen(location)+5);	/* be generous */
     strcpy (backup_location, location);
     strcat (backup_location, "~");

     future_location = malloc (strlen(location)+5);	/* be generous */
     strcpy (future_location, location);
     strcat (future_location, "#");

     printf("Creating new meeting\n");
     fflush(stdout);

     if (mtg_name == NULL) {
	  mtg_name = old_mtg_info.long_name;
     }
     if (chairman == NULL) {
	  chairman = old_mtg_info.chairman;
     }

     /* get acl's on old meeting, so we can make it new one */
     get_acl (location, &result, &new_acl_list);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting acl\n", backup_location, error_message(result));
	  exit (1);
     }

     create_mtg_priv (backup_location, mtg_name, old_mtg_info.public_flag,
		      old_mtg_info.date_created, chairman,
		      new_acl_list, &result);
     if (result != 0) {
	  fprintf (stderr, "%s: %s while creating new meeting\n",
		   location, error_message(result));
	  exit (1);
     }
     
     /* now, do the actual expunging */
     low = old_mtg_info.lowest;
     high = old_mtg_info.highest;
     create_temp ();

expunge_range:
     for (i = low; i <= high; i++) {
	  get_trn_info3 (location, i, &old_trn_info, &result);
	  if (result != 0 && result != DELETED_TRN && result != EXPUNGED_TRN) {
	       fprintf(stderr,
		       "Error getting info for transaction [%04d]: %s\n",
		       i, error_message(result));
	       error_occurred = TRUE;
	  } else if (result != 0) {		/* expunge it */
	       no_nuke = TRUE;
	       printf("Expunging transaction [%04d]\n", i);
	       expunge_trn (backup_location, i, &result);
	       no_nuke = FALSE;
	       if (result != 0) {
		    fprintf(stderr,
			    "Error expunging transaction [%04d]: %s\n",
			    i, error_message(result));
		    error_occurred = TRUE;
	       }
	  } else if (result == 0) {
	       ftruncate(tempf,0);
	       lseek(tempf,0,0);
	       tf = unix_tfile (tempf);

	       get_trn (location, i, tf, &result);
	       if (result != 0) {
		    fprintf(stderr, "Error getting transaction [%04d]: %s\n",
			    i, error_message(result));
		    error_occurred = TRUE;
		    free(old_trn_info. author);
		    free(old_trn_info.subject);
		    continue;
	       }

	       tdestroy (tf);

	       signature = old_trn_info.signature;
	       if (daemon_flag && old_trn_info.signature != NULL) {
#ifdef POSIX
		    memmove(dtest, old_trn_info.signature,  7);
#else
		    bcopy(old_trn_info.signature, dtest, 7);
#endif
		    dtest[7] = '\0';
		    if (!strcmp(dtest,"daemon@")) {
			 if (get_from_signature(tempf, new_signature, sizeof(new_signature)))
			      signature = new_signature;
		    }
	       }
	       lseek(tempf,0,0);
	       tf = unix_tfile (tempf);
	       no_nuke = TRUE;
	       add_trn_priv (backup_location, tf, old_trn_info.subject,
			     signature, old_trn_info.pref,
			     old_trn_info.current, old_trn_info.author,
			     old_trn_info.date_entered, old_trn_info.flags,
			     &n, &result);
	       no_nuke = FALSE;
	       if (result != 0) {
		    fprintf(stderr,
			    "Error getting info for transaction %d: %s\n", i,
			    error_message(result));
		    error_occurred = TRUE;
	       }
	       tdestroy(tf);
	       free(old_trn_info.author);
	       free(old_trn_info.subject);
	       free(old_trn_info.signature);
	  }
     }

     /* Check if any new transactions have been added */
     free(old_mtg_info.long_name);
     free(old_mtg_info.chairman);
     free(old_mtg_info.location);
     get_mtg_info(location, &old_mtg_info, &result);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting mtg info\n", location, error_message(result));
	  error_occurred = TRUE;
     } else if (old_mtg_info.highest > high) {		/* New transactions added */
	  low = high+1;
	  high = old_mtg_info.highest;
	  goto expunge_range;
     }

     strcpy(control_name, location);
     strcat(control_name, "/control");
     if ((control_fd = open (control_name, O_RDWR, 0700)) < 0) {
	  error_occurred = TRUE;
     } else {
#ifdef SOLARIS
          lock.l_type = F_WRLCK;
          lock.l_start = 0;
          lock.l_whence = 0;
          lock.l_len = 0;
          fcntl(control_fd, F_SETLK, &lock);
#else
	  flock(control_fd, LOCK_EX);		/* Hold meeting by the balls */
#endif
	  free(old_mtg_info.long_name);
	  free(old_mtg_info.chairman);
	  free(old_mtg_info.location);

	  no_nuke = TRUE;
	  get_mtg_info(location, &old_mtg_info, &result);
	  if (result != 0) {	       
	       fprintf(stderr, "%s: %s while getting mtg info\n", location, error_message(result));
	       error_occurred = TRUE;
#ifdef SOLARIS
             lock.l_type = F_UNLCK;
             lock.l_start = 0;
             lock.l_whence = 0;
             lock.l_len = 0;
             fcntl(control_fd, F_SETLK, &lock);
#else
	       flock(control_fd,LOCK_UN);
#endif
	       close(control_fd);
	  } else if (old_mtg_info.highest > high) {		/* New transactions added */
	       low = high + 1;
	       high = old_mtg_info.highest;
#ifdef POSIX
               lock.l_type = F_UNLCK;
               lock.l_start = 0;
               lock.l_whence = 0;
               lock.l_len = 0;
               fcntl(control_fd, F_SETLK, &lock);
#else
	       flock(control_fd,LOCK_UN);
#endif
	       close(control_fd);
	       goto expunge_range;
	  }
     }

     /* When we get here, we have the old meeting locked.  Now we do the move
	as atomically as we can */
     if (!error_occurred) {
	  if (rename(location, future_location) < 0) {
	       perror("rename of old meeting failed");
	       exit (1);
	  }
	  if (rename(backup_location, location) < 0) {
	       perror("rename of new meeting");
	       exit (1);
	  }
	  remove_mtg (future_location, &result);
	  if (result != 0) {
	       fprintf(stderr, "%s: %s while removing new meeting.\n",
		       location, error_message(result));
	       exit (1);
	  }
     } else exit (1);				/* error occurred */

     exit (0);

lusage:
     fprintf(stderr, "usage: expunge mtg_location {-c chairman} {-n name}\n");
     exit (1);
}

/*
 *
 * create_temp () -- Create temp file, and let it be tempf.
 *
 */
create_temp()
{
     char *filename;

     filename = malloc (strlen (temp_dir) + 20);
     strcpy (filename, temp_dir);
     strcat (filename, "/rcXXXXXX");
     mktemp (filename);

     tempf = open (filename, O_RDWR | O_CREAT, 0700);
     if (tempf < 0) {
	  fprintf (stderr, "Cannot open temp file\n");
	  exit (1);
     }
}

/*
 *
 *   get_from_signature () -- Look in the file file_no, looking for a
 *	From: line.  Construct a signature from this from line, returning
 *	it in sign, with a maximum length of sign_len.
 *
 */

int
get_from_signature(file_no, sign, sign_len)
int file_no;
char *sign;
int sign_len;
{
     char buf[2048];
     int len;
     char *cp, *hp;

     lseek(file_no, 0, 0);
     len = read(file_no, buf, sizeof(buf)-1);
     buf[len] = '\0';

     hp = get_header(buf, "from");
     if (hp == NULL)
	  return(0);

     cp = strchr(hp, ':');
     if (cp == NULL)
	  return(0);

     cp++;

     extract_full_name(cp, sign, sign_len);
     return(1);
}

/* Stolen from TechMail */

static
lower(s)
char *s;
{
     while (*s) {
	  if (isupper(*s))
	       *s = tolower(*s);
	  s++;
     }
     return;
}

static
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

static
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

static char *
get_header(hstring, hname)
char *hstring, *hname;
{
     char *eolp, *colonp, *cp;
     int name_len;
     char field[33];

     cp = hstring;
     name_len = strlen(hname);

     while (*cp != '\0') {
	  eolp = strchr(cp, '\n');
	  colonp = strchr(cp, ':');

	  if (eolp == NULL || colonp == NULL || eolp == cp)
	       return(0);

	  if (colonp > eolp || colonp - cp != name_len) {
	       cp = eolp+1;
	       continue;
	  }

	  /* Chance of a match, copy header over to name, and lower it */
#ifdef POSIX
	  memmove(field,cp,  colonp - cp);
#else
	  bcopy(cp, field, colonp - cp);
#endif
	  field[colonp - cp] = '\0';
	  lower(field);
	  if (!strcmp(field,hname))
	       return(cp);
	  cp = eolp + 1;
     }

     return(NULL);
}
