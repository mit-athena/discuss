/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/coreutil.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/coreutil.c,v 1.7 1987-03-25 15:04:31 srz Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *
 *
 * coreutil.c  -- These contain lower-layer, utility type routines to
 *		  be used by core.  These include things to handle the
 *		  in-memory superblock, and to open & close meetings.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.6  87/03/17  02:24:10  srz
 * Added expunging.  An ACL change will require meeting to be reopened, in
 * case an expunge is taking place.  Also added has_privs, which allows
 * wheel access for programs that are linked in.
 * 
 * Revision 1.5  87/03/11  18:00:27  srz
 * Made sure that write's were error checked.
 * 
 * Revision 1.4  87/02/04  15:48:18  srz
 * When there is a choice between making lint happy or cc happy, I tend
 * to prefer 'cc'.  uid_t is not in 4.2 BSD.
 * 
 * Revision 1.3  86/11/22  06:25:42  spook
 * Changed to make lint happy.
 * 
 * Revision 1.2  86/11/16  06:05:37  wesommer
 * Changed open_mtg to stat(2) the acl file before assuming that it's 
 * up to date, and revert the acl from there if the in-core version
 * is stale.
 * 
 */

#ifndef lint
static char *rcsid_coreutil_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/coreutil.c,v 1.7 1987-03-25 15:04:31 srz Exp $";
#endif lint

#include "../include/types.h"
#include "../include/dsc_et.h"
#include "../include/atom.h"
#include "mtg.h"
#include "../include/tfile.h"
#include "../include/acl.h"
#include <errno.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <strings.h>

#define NULL 0

/* global variables */
char current_mtg [256] = "";			/* meeting that's opened */
int u_trn_f,u_control_f,u_acl_f;		/* UNIX file descriptors */
bool nuclear = FALSE;				/* Using atomic reads/writes */
afile a_control_f = NULL;			/* radioactive file descriptor */
tfile abort_file = NULL;			/* close this on abort */
bool  read_lock = FALSE;			/* have lock on u_control_f */
Acl   *mtg_acl = NULL;				/* current mtg acl */
int 	last_acl_mod;				/* last mod to ACL */
mtg_super super;
char *super_chairman;
char *super_long_name;
int has_privs = 0;


/* EXTERNAL */
extern char *malloc();
extern off_t lseek();
extern int errno;
extern char rpc_caller [];

/*
 *
 * new_string (s)  --   Routine to create a copy of the given string, using
 *		      	malloc.
 *
 */
char *new_string (s)
char *s;
{
     int len;
     char *newstr;

     len = strlen (s) + 1;
     newstr = malloc ((unsigned)len);
     (void) strcpy (newstr, s);
     return (newstr);
}

/*
 *
 * open_mtg (mtg_name) -- Routine to open a meeting, if necessary.
 *
 */
int open_mtg (mtg_name)
char *mtg_name;
{
     char str[256];
     int result;
     int mtg_name_len;
     trn_base tb;
     int uid = (int)geteuid();
     struct stat sb;

     mtg_name_len = strlen (mtg_name);
     if (mtg_name[0] != '/' || mtg_name_len == 0 || mtg_name_len > 168 || mtg_name [mtg_name_len-1] == '/') {
	  return (BAD_PATH);
     }

     (void) strcpy (str, mtg_name);
     (void) strcat (str, "/acl");

     if (!strcmp (mtg_name, current_mtg)) {
	  /*
	   * is acl stale? 
	   */
	  if (stat(str, &sb) >= 0)
	       if (sb.st_mtime <= last_acl_mod)
		    return (0);				/* that was easy */
     }

     if (current_mtg [0] != '\0') {		/* close previous meeting */
	  if (nuclear)
	       panic ("Nuclear flag error");	/* should never happen */
	  (void) close (u_trn_f);
	  (void) close (u_control_f);
	  current_mtg [0] = '\0';
	  acl_destroy(mtg_acl);
     }

     u_trn_f = u_control_f = u_acl_f = 0;

     if ((u_acl_f = open(str, O_RDONLY, 0700)) < 0) {
	  if (errno == ENOENT)
	       result = NO_SUCH_MTG;
	  else if (errno == EACCES)
	       result = NO_ACCESS;
	  else
	       result = BAD_PATH;
	  goto punt;
     }
     if (!fis_owner (u_acl_f, uid)) {
	  result = NO_ACCESS;
	  goto punt;
     }

     mtg_acl = acl_read (u_acl_f);
     (void) fstat(u_acl_f, &sb);
     last_acl_mod = sb.st_mtime;
     (void) close(u_acl_f);
     u_acl_f = 0;

     (void) strcpy (str, mtg_name);
     (void) strcat (str, "/control");

     if ((u_control_f = open(str, O_RDWR, 0700)) < 0) {
	  if (errno == ENOENT)
	       result = NO_SUCH_MTG;
	  else if (errno == EACCES)
	       result = NO_ACCESS;
	  else
	       result = BAD_PATH;
	  goto punt;
     }

     if (!fis_owner (u_control_f, uid)) {
	  result = NO_ACCESS;
	  goto punt;
     }

     (void) strcpy (str, mtg_name);
     (void) strcat (str, "/transactions");
     
     if ((u_trn_f = open(str, O_RDWR, 0700)) < 0) {
	  if (errno == ENOENT)
	       result = NO_SUCH_MTG;
	  else if (errno == EACCES)
	       result = NO_ACCESS;
	  else
	       result = BAD_PATH;
	  goto punt;
     }

     if (!fis_owner (u_trn_f, uid)) {
	  result = NO_ACCESS;
	  goto punt;
     }

     read (u_trn_f, (char *) &tb, sizeof (trn_base));
     if (tb.unique != TRN_BASE_UNIQUE) {
	  result = INCONSISTENT;
	  goto punt;
     }

     (void) strcpy (current_mtg, mtg_name);

     return (0);

 punt:
     if (u_trn_f)
	     (void) close(u_trn_f);
     if (u_control_f)
	     (void) close(u_control_f);
     if (u_acl_f)
	     (void) close(u_acl_f);
     acl_destroy(mtg_acl);
     mtg_acl = NULL;
     return (result);
}


int read_super()
{
     if (nuclear) {
	  aread (a_control_f, (char *) &super, sizeof(super), 0);
     } else {
	     lseek (u_control_f, (long)0, 0);
	     read (u_control_f, (char *) &super, sizeof (super));
     }

     if (super.version != MTG_SUPER_1)
	  return (NEW_VERSION);

     if (super.unique != MTG_SUPER_UNIQUE)
	  return (INCONSISTENT);

     super_long_name = malloc ((unsigned)super.long_name_len);
     super_chairman = malloc ((unsigned)super.chairman_len);

     if (nuclear) {
	  aread (a_control_f, super_long_name, super.long_name_len, super.long_name_addr);
	  aread (a_control_f, super_chairman, super.chairman_len, super.chairman_addr);
     } else {
	  lseek (u_control_f, (long)super.long_name_addr, 0);
	  read (u_control_f, super_long_name, super.long_name_len);
	  lseek (u_control_f, (long)super.chairman_addr, 0);
	  read (u_control_f, super_chairman, super.chairman_len);
     }

     return(0);
}

write_super ()
{
     int sc_len, sl_len;

     sc_len = strlen (super_chairman)+1;
     sl_len = strlen (super_long_name)+1;

     if (sc_len != super.chairman_len || sl_len != super.long_name_len) {		/* reallocate things */
	  super.long_name_addr = sizeof (super);
	  super.long_name_len = sl_len;
	  super.chairman_addr = super.long_name_addr + super.long_name_len;
	  super.chairman_len = sc_len;
     }

     if (nuclear) {
	  awrite (a_control_f, (char *) &super, sizeof (super), 0);
	  awrite (a_control_f, super_long_name, super.long_name_len, super.long_name_addr);
	  awrite (a_control_f, super_chairman, super.chairman_len, super.chairman_addr);
     } else {
	  lseek (u_control_f, (long)0, 0);
	  write (u_control_f, (char *)  &super, sizeof (super));
	  lseek (u_control_f, (long)super.long_name_addr, 0);
	  write (u_control_f, super_long_name, super.long_name_len);
	  lseek (u_control_f, (long)super.chairman_addr, 0);
	  write (u_control_f, super_chairman, super.chairman_len);
     }

     free (super_chairman);
     free (super_long_name);
     super.unique = 0;					/* prevent accidents */

     return;
}

/*
 *
 * forget_super () -- Routine to dump superblock, like when we just read
 *		      it.  This frees any alloc'd storage.
 *
 */
forget_super()
{
     free(super_long_name);
     free(super_chairman);

     super.unique = 0;
}

/*
 *
 * start_read () --	This reserves the meeting for non-destructive reading.
 *			Simply does an flock.
 *
 */
start_read()
{
     if (flock (u_control_f, LOCK_SH) < 0)
	  panic ("Cannot share lock");
     read_lock = TRUE;
}

/*
 *
 * finish_read () --	This frees up our reservation on the meeting.  This
 *			does the opposite of start_read.
 *
 */
finish_read()
{
     flock(u_control_f,LOCK_UN);
     read_lock = 0;
}

/*
 *
 * chain_addr -- Returns address of given chain block.
 *
 */
faddr chain_addr(trn)
trn_nums trn;
{
     if (trn < super.lowest || trn > super.highest)
	  return (0);

     return (super.chain_start + (trn-1) * sizeof (chain_blk));
}

int read_chain (trn, cb)
trn_nums trn;
chain_blk *cb;
{
     faddr cbaddr;

     cbaddr = chain_addr (trn);
     if (cbaddr == 0)
	  return (NO_SUCH_TRN);

     if (nuclear)
	  aread (a_control_f, (char *) cb, sizeof (chain_blk), cbaddr);
     else {
	  lseek (u_control_f, (long)cbaddr, 0);
	  read (u_control_f, (char *) cb, sizeof (chain_blk));
     }

     if (cb -> version != CHAIN_BLK_1)
	  return (NEW_VERSION);

     if (cb -> unique != CHAIN_BLK_UNIQUE || cb -> current != trn)
	  return (INCONSISTENT);

     return (0);
}

int write_chain (cb)
chain_blk *cb;
{
     faddr cbaddr;

     cbaddr = chain_addr (cb -> current);
     if (cbaddr == 0)
	  return (NO_SUCH_TRN);

     if (nuclear) {
	  if (awrite (a_control_f, (char *) cb, sizeof (chain_blk), cbaddr) != sizeof (chain_blk))
	       return (NO_WRITE);
     }
     else {
	  lseek (u_control_f, (long)cbaddr, 0);
	  if (write (u_control_f, (char *) cb, sizeof (chain_blk)) != sizeof (chain_blk))
	       return (NO_WRITE);
     }

     return (0);
}

/*
 *
 * read_trn -- routine to read a transaction from the transaction file.
 *
 */
int read_trn (trn_addr, th, th_subject, th_author)
faddr trn_addr;
trn_hdr *th;
char **th_subject, **th_author;
{
     lseek (u_trn_f, (long)trn_addr, 0);
     read (u_trn_f, (char *) th, sizeof (trn_hdr));

     if (th -> version != TRN_HDR_1)
	  return(NEW_VERSION);

     if (th -> unique != TRN_HDR_UNIQUE)
	  return (INCONSISTENT);

     if (th_subject != 0) {
	  *th_subject = malloc ((unsigned)(th -> subject_len));
	  lseek (u_trn_f, (long)(th -> subject_addr), 0);
	  read (u_trn_f, *th_subject, th -> subject_len);
     }
	  
     if (th_author != 0) {
	  *th_author = malloc ((unsigned)(th -> author_len));
	  lseek (u_trn_f, (long)(th -> author_addr), 0);
	  read (u_trn_f, *th_author, th -> author_len);
     }

     return(0);
}
	  

/*
 *
 * core_abort () -- Routine for use by core routines to clean things up.
 *
 */
core_abort ()
{
     int dummy;

     if (nuclear) {
	  aabort(a_control_f);
	  nuclear = FALSE;
     }

     if (abort_file != NULL) {
	  tclose (abort_file,&dummy);
	  abort_file = NULL;
     }

     if (read_lock)
	  finish_read();

     return;
}

/*
 *
 * fsize () -- Routine to find out the size of a file.
 *
 */
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
 * fis_owner () -- Routine to find out if uid is owner of file.
 *
 */
fis_owner (d, uid)
int d,uid;
{
     struct stat buf;

     if (fstat (d, &buf) < 0)
	  return (0);

     return (uid == buf.st_uid);
}

/*
 *
 * has_trn_access -- Routine to return true if current user can access
 *		     transaction.
 *
 */
bool has_trn_access(author,mode)
char *author;
char mode;
{
     char *test_modes;

     if (has_privs)
	  return(TRUE);				/* linked in */

     switch (mode) {
     case 'd':
	  if (!strcmp(author,rpc_caller)) {
	       test_modes = "o";
	  } else {
	       test_modes = "d";
	  }
	  return (acl_check(mtg_acl,rpc_caller,test_modes));

     case 'r':
	  return (acl_check(mtg_acl,rpc_caller,"r") ||
		  ((!strcmp (author, rpc_caller)) && acl_check(mtg_acl,rpc_caller,"o")));
     default:
	  panic ("Invalid mode");
	  /*NOTREACHED*/
     }
}	  
/*
 *
 * has_mtg_access -- Routine to return true if current user can access
 *		     mtg, with given mode.
 *
 */
bool has_mtg_access(mode)
char mode;
{
     char *test_modes;
     char mode_str[3];

     if (has_privs)				/* linked in stuff */
	  return (TRUE);		

     switch (mode) {
     case 'w':
     case 'a':
     case 'r':
     case 's':
     case 'o':
	  mode_str[0] = mode;
	  mode_str[1] = '\0';
	  test_modes = mode_str;
	  break;

     case 'c':
	  if (!strcmp (super_chairman, rpc_caller))
	       return (TRUE);
	  test_modes = "c";
	  break;

     default:
	  panic("Invalid mode");
     }

     return(acl_check(mtg_acl,rpc_caller,test_modes));
}	  
/*
 *
 * panic -- just a printf
 *
 */
panic(str)
char *str;
{
     printf("panic: %s\n",str);
     perror("discuss");
     exit(1);
}
