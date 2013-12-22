/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: coreutil.c,v 1.32 2002-04-06 15:18:55 zacheiss Exp $
 *
 *
 * coreutil.c  -- These contain lower-layer, utility type routines to
 *		  be used by core.  These include things to handle the
 *		  in-memory superblock, and to open & close meetings.
 *
 */

#ifndef lint
#ifdef __STDC__
const
#endif
static char rcsid_coreutil_c[] =
    "$Id: coreutil.c,v 1.32 2002-04-06 15:18:55 zacheiss Exp $";
#endif /* lint */

#include <discuss/types.h>
#include <discuss/dsc_et.h>
#include "atom.h"
#include "mtg.h"
#include <discuss/tfile.h>
#include <discuss/acl.h>
#include "internal.h"
#include "ansi.h"
#if HAVE_ZEPHYR
#include <zephyr/zephyr.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <string.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <unistd.h>

/* global variables */
char current_mtg [256] = "";	/* meeting that's opened */
int u_trn_f,u_control_f,u_acl_f; /* UNIX file descriptors */
bool nuclear = FALSE;		/* Using atomic reads/writes */
afile a_control_f = NULL;	/* radioactive file descriptor */
tfile abort_file = NULL;	/* close this on abort */
bool  read_lock = FALSE;	/* have lock on u_control_f */
bool  mtg_swapped = FALSE;	/* current meeting is swapped */
dsc_acl   *mtg_acl = NULL;	/* current mtg acl */
int 	last_acl_mod;		/* last mod to ACL */
mtg_super super;
char *super_chairman;
char *super_long_name;
int has_privs = 0;		/* Has privileges (linked) */
int no_nuke = 0;		/* Don't be atomic (linked) */
#if HAVE_ZEPHYR
int use_zephyr = 1;		/* Actually do use Zephyr. */
#else
int use_zephyr = 0;
#endif


/* EXTERNAL */
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
     if (mtg_name[0] != '/' || mtg_name_len == 0 || mtg_name_len > 168
	 || mtg_name [mtg_name_len-1] == '/') {
	  return (BAD_PATH);
     }

     /* Check for moved meeting */
     (void) strcpy (str, mtg_name);
     (void) strcat (str, "/forward");
     if (!stat(str, &sb)) {
	  return(MTG_MOVED);
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
	  mtg_acl = NULL;
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
     if (tb.unique == TRN_BASE_UNIQUE_SWAP) {
	  swap_trn_base(&tb);
	  mtg_swapped = TRUE;
     } else {
	  mtg_swapped = FALSE;
     }

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
	     lseek (u_control_f, (long)0, SEEK_SET);
	     read (u_control_f, (char *) &super, sizeof (super));
     }

     if (!mtg_swapped) {
	  if (super.unique != MTG_SUPER_UNIQUE)
	       return (INCONSISTENT);
     } else {
	  if (super.unique != MTG_SUPER_UNIQUE_SWAP)
	       return (INCONSISTENT);
	  swap_super(&super);
     }

     if (super.version != MTG_SUPER_1)
	  return (NEW_VERSION);

     super_long_name = malloc ((unsigned)super.long_name_len);
     super_chairman = malloc ((unsigned)super.chairman_len);

     if (nuclear) {
	  aread (a_control_f, super_long_name, super.long_name_len, super.long_name_addr);
	  aread (a_control_f, super_chairman, super.chairman_len, super.chairman_addr);
     } else {
	  lseek (u_control_f, (long)super.long_name_addr, SEEK_SET);
	  read (u_control_f, super_long_name, super.long_name_len);
	  lseek (u_control_f, (long)super.chairman_addr, SEEK_SET);
	  read (u_control_f, super_chairman, super.chairman_len);
     }

     return(0);
}

write_super ()
{
     int sc_len, sl_len;
     int slong_name_len, slong_name_addr;
     int schairman_len, schairman_addr;

     sc_len = strlen (super_chairman)+1;
     sl_len = strlen (super_long_name)+1;

     if (sc_len != super.chairman_len || sl_len != super.long_name_len) {		/* reallocate things */
	  super.long_name_addr = sizeof (super);
	  super.long_name_len = sl_len;
	  super.chairman_addr = super.long_name_addr + super.long_name_len;
	  super.chairman_len = sc_len;
     }

     /* Copy into locals to avoid swapping */
     slong_name_len = super.long_name_len;
     slong_name_addr = super.long_name_addr;
     schairman_len = super.chairman_len;
     schairman_addr = super.chairman_addr;

     if (mtg_swapped)
	  swap_super(&super);

     if (nuclear) {
	  awrite (a_control_f, (char *) &super, sizeof (super), 0);
	  awrite (a_control_f, super_long_name, slong_name_len, slong_name_addr);
	  awrite (a_control_f, super_chairman, schairman_len, schairman_addr);
     } else {
	  lseek (u_control_f, (long)0, SEEK_SET);
	  write (u_control_f, (char *)  &super, sizeof (super));
	  lseek (u_control_f, (long)slong_name_addr, SEEK_SET);
	  write (u_control_f, super_long_name, slong_name_len);
	  lseek (u_control_f, (long)schairman_addr, SEEK_SET);
	  write (u_control_f, super_chairman, schairman_len);
     }

     free (super_chairman);
     super_chairman = NULL;
     free (super_long_name);
     super_long_name = NULL;
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
     if (super_long_name != NULL)
	  free(super_long_name);
     super_long_name = NULL;

     if (super_chairman != NULL)
	  free(super_chairman);
     super_chairman = NULL;
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
     struct flock lock;

     if (!no_nuke) {
          lock.l_type = F_RDLCK;
          lock.l_start = 0;
          lock.l_whence = 0;
          lock.l_len = 0;
          if (fcntl(u_control_f, F_SETLKW, &lock) < 0) 
	       panic ("Cannot share lock");
	  read_lock = TRUE;
     }
}

/*
 *
 * finish_read () --	This frees up our reservation on the meeting.  This
 *			does the opposite of start_read.
 *
 */
finish_read()
{
    struct flock lock;

     if (!no_nuke) {
           lock.l_type = F_UNLCK;
          lock.l_start = 0;
          lock.l_whence = 0;
          lock.l_len = 0;
          fcntl(u_control_f, F_SETLK, &lock);
	  read_lock = 0;
     }
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
	  lseek (u_control_f, (long)cbaddr, SEEK_SET);
	  read (u_control_f, (char *) cb, sizeof (chain_blk));
     }

     if (mtg_swapped)
	  swap_chain(cb);

     if (cb -> unique != CHAIN_BLK_UNIQUE || cb -> current != trn)
	  return (INCONSISTENT);

     if (cb -> version != CHAIN_BLK_1)
	  return (NEW_VERSION);

     return (0);
}

int write_chain (cb)
chain_blk *cb;
{
     faddr cbaddr;

     cbaddr = chain_addr (cb -> current);
     if (cbaddr == 0)
	  return (NO_SUCH_TRN);

     if (mtg_swapped)
	  swap_chain(cb);

     if (nuclear) {
	  if (awrite (a_control_f, (char *) cb, sizeof (chain_blk), cbaddr) != sizeof (chain_blk)) {
	       if (mtg_swapped)
		    swap_chain(cb);
	       return (NO_WRITE);
	  }
     }
     else {
	  lseek (u_control_f, (long)cbaddr, SEEK_SET);
	  if (write (u_control_f, (char *) cb, sizeof (chain_blk)) != sizeof (chain_blk)) {
	       if (mtg_swapped)
		    swap_chain(cb);
	       return (NO_WRITE);
	  }
     }

     if (mtg_swapped)
	  swap_chain(cb);
     return (0);
}

/*
 *
 * read_trn -- routine to read a transaction from the transaction file.
 *
 */
int read_trn (trn_addr, th, th_subject, th_author, th_signature)
faddr trn_addr;
trn_hdr *th;
char **th_subject, **th_author, **th_signature;
{
     char *author;

     lseek (u_trn_f, (long)trn_addr, SEEK_SET);
     read (u_trn_f, (char *) th, sizeof (trn_hdr));

     if (mtg_swapped)
	  swap_trn(th);

     if (th -> unique != TRN_HDR_UNIQUE)
	  return (INCONSISTENT);

     if (th -> version != TRN_HDR_1)
	  return(NEW_VERSION);

     if (th_subject != NULL) {
	  *th_subject = malloc ((unsigned)(th -> subject_len));
	  lseek (u_trn_f, (long)(th -> subject_addr), SEEK_SET);
	  read (u_trn_f, *th_subject, th -> subject_len);
     }
	  
     if (th_author != NULL || th_signature != NULL) {
	  author = malloc ((unsigned)(th -> author_len));
	  lseek (u_trn_f, (long)(th -> author_addr), SEEK_SET);
	  read (u_trn_f, author, th -> author_len);
     }

     if (th_author != NULL)
	  *th_author = author;

     if (th_signature != NULL) {
	  if (strlen(author)+1 == th -> author_len) {
	       *th_signature = new_string(author);	/* No signature, return author */
	  } else {
	       *th_signature = new_string(&author[strlen(author)+1]);
	  }
	  if (th_author == NULL)
	       free(author);
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

     forget_super();

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

#if HAVE_ZEPHYR
/*
 *
 * mtg_znotify -- send off a Zephyr notification as appropriate
 *
 */

static const char * this_host = (const char *) NULL;

void mtg_znotify(mtg_name, subject, author, signature)
	char *mtg_name, *subject, *author, *signature;
{
	register dsc_acl_entry *ae;
	register int n;
	ZNotice_t notice;
	char *msglst[5],bfr[30],fullpath[256];
	int code, list_size;

	if (!use_zephyr)
	    return;

	if (!this_host) {
	    /* perform initializations */
	    char *h;
	    char host[100];
	    struct hostent *hent;
	    if (gethostname(host,100) != 0)
		return;
	    hent = (struct hostent *) gethostbyname(host);
	    if (hent == 0)
		return;
	    h = (char *) malloc (strlen (hent->h_name) + 1);
	    if (!h)
		return;
	    strcpy (h, hent->h_name);
	    this_host = h;
	    ZInitialize();
	}

	/* Set up the notice structure */
	memset(&notice, 0, sizeof(notice));

	sprintf(fullpath,"%s:%s", this_host, mtg_name);
	ZOpenPort(NULL);
	notice.z_kind = UNSAFE;
	notice.z_port = 0;
	notice.z_class = "DISCUSS";
	notice.z_class_inst = fullpath;
	notice.z_opcode = "NEW_TRN";
	notice.z_sender = 0;
	if (signature == NULL)
	     notice.z_default_format = "New transaction [$1] entered in $2\nFrom: $3\nSubject: $4";
	else
	     notice.z_default_format = "New transaction [$1] entered in $2\nFrom: $3 ($5)\nSubject: $4";
	msglst[0] = bfr;
	sprintf(msglst[0],"%04d",super.highest);
	msglst[1] = super_long_name;
	msglst[2] = author;
	msglst[3] = subject;
	list_size = 4;
	if (signature != NULL) {
	     msglst[4] = signature;
	     list_size = 5;
	}

	/* Does "*" have read access? If so, just send out a global
	 * notice.
	 */

	/* XXX
	 * Check at some point for people who don't have access, etc.
	 */

	for (ae = mtg_acl->acl_entries, n=mtg_acl->acl_length;
	     n;
	     ae++, n--) {
		if ((strcmp("*", ae->principal) == 0) &&
		    acl_is_subset("r", ae->modes))
			break;
	}
	if (n) {
		notice.z_recipient = "";
		/* We really don't care if it gets through... */
		code = ZSendList(&notice,msglst,list_size,ZNOAUTH);
		return;
	}
	for (ae = mtg_acl->acl_entries, n=mtg_acl->acl_length;
	     n;
	     ae++, n--) {
		if (acl_is_subset("r", ae->modes)) {
			notice.z_recipient = ae->principal;
			ZSendList(&notice,msglst,list_size,ZNOAUTH);
		}
	}
}
#endif

static unsigned long swap_32(val)
unsigned long val;
{
     unsigned char b1 = (val >> 24) & 0xff;
     unsigned char b2 = (val >> 16) & 0xff;
     unsigned char b3 = (val >> 8) & 0xff;
     unsigned char b4 = val & 0xff;

     return ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
}

static unsigned int swap_16(val)
unsigned int val;
{
     unsigned char b1 = (val >> 8) & 0xff;
     unsigned char b2 = val & 0xff;

     return ((b2 << 8) | b1);
}

swap_super(superp)
mtg_super *superp;
{
     superp->version = swap_32(superp->version);
     superp->unique = swap_32(superp->unique);
     superp->first = swap_32(superp->first);
     superp->last = swap_32(superp->last);
     superp->lowest = swap_32(superp->lowest);
     superp->highest = swap_32(superp->highest);
     superp->highest_chain = swap_32(superp->highest_chain);
     superp->date_created = swap_32(superp->date_created);
     superp->date_modified = swap_32(superp->date_modified);
     superp->long_name_addr = swap_32(superp->long_name_addr);
     superp->chairman_addr = swap_32(superp->chairman_addr);
     superp->long_name_len = swap_16(superp->long_name_len);
     superp->chairman_len = swap_16(superp->chairman_len);
     superp->flags = swap_16(superp->flags);
     superp->chain_start = swap_32(superp->chain_start);
     superp->high_water = swap_32(superp->high_water);
     superp->trn_fsize = swap_32(superp->trn_fsize);
     superp->highest_trn_addr = swap_32(superp->highest_trn_addr);
}

swap_chain(cbp)
chain_blk *cbp;
{
     cbp->version = swap_32(cbp->version);
     cbp->unique = swap_32(cbp->unique);
     cbp->current = swap_32(cbp->current);
     cbp->prev = swap_32(cbp->prev);
     cbp->next = swap_32(cbp->next);
     cbp->pref = swap_32(cbp->pref);
     cbp->nref = swap_32(cbp->nref);
     cbp->trn_chain = swap_32(cbp->trn_chain);
     cbp->trn_addr = swap_32(cbp->trn_addr);
     cbp->flags = swap_16(cbp->flags);
     cbp->filler = swap_16(cbp->filler);
     cbp->chain_fref = swap_32(cbp->chain_fref);
     cbp->chain_lref = swap_32(cbp->chain_lref);
}

swap_trn_base(tbp)
trn_base *tbp;
{
     tbp->version = swap_32(tbp->version);
     tbp->unique = swap_32(tbp->unique);
     tbp->date_created = swap_32(tbp->date_created);
     tbp->long_name_addr = swap_32(tbp->long_name_addr);
     tbp->chairman_addr = swap_32(tbp->chairman_addr);
     tbp->long_name_len = swap_16(tbp->long_name_len);
     tbp->chairman_len = swap_16(tbp->chairman_len);
     tbp->public_flag = swap_16(tbp->public_flag);
}

swap_trn(thp)
trn_hdr *thp;
{
     thp->version = swap_32(thp->version);
     thp->unique = swap_32(thp->unique);
     thp->current = swap_32(thp->current);
     thp->orig_pref = swap_32(thp->orig_pref);
     thp->date_entered = swap_32(thp->date_entered);
     thp->num_lines = swap_32(thp->num_lines);
     thp->num_chars = swap_32(thp->num_chars);
     thp->prev_trn = swap_32(thp->prev_trn);
     thp->subject_addr = swap_32(thp->subject_addr);
     thp->author_addr = swap_32(thp->author_addr);
     thp->text_addr = swap_32(thp->text_addr);
     thp->subject_len = swap_16(thp->subject_len);
     thp->author_len = swap_16(thp->author_len);
}
