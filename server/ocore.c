/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/ocore.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/ocore.c,v 1.1 1993-10-12 05:59:10 probe Exp $
 *
 *
 * core.c --    Routines that are the meat of discuss.  These provide user
 *		callable routines.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.30  90/09/11  19:02:59  srz
 * Added per-meeting Zephyr flag (to be patched for the moment).
 * 
 * Revision 1.29  90/03/19  13:38:51  srz
 * Fixed bug where rmds would cause reset connections as a sub-process.
 * 
 * Revision 1.28  90/02/24  18:58:08  srz
 * Added signatures to meetings.  This involves adding two routines,
 * add_trn2 and get_trn_info3, for storing and saving this information.
 * Signatures are glommed onto the end of the author, so we don't
 * have to change the meeting format.
 * 
 * Revision 1.27  89/08/09  22:39:05  srz
 * Added meeting forwarding.
 * 
 * Revision 1.26  89/06/03  00:42:11  srz
 * Added standard copyright notice.
 * 
 * Revision 1.25  89/01/29  17:17:12  srz
 * Added flag routines.
 * 
 * Revision 1.24  89/01/04  23:32:51  raeburn
 * fixed include paths
 * 
 * Revision 1.23  88/10/08  01:27:54  srz
 * Changes for new expunge.
 * 
 * Revision 1.22  88/09/23  17:15:21  raeburn
 * Needs internal.h too.
 * 
 * Revision 1.21  88/09/23  17:05:04  raeburn
 * Changed type names in accordance with acl.h.
 * 
 * Revision 1.20  88/06/17  23:12:43  srz
 * Change update_mtg to return an error if the meeting appears to have
 * lost transactions.
 * 
 * Revision 1.19  88/01/05  01:08:44  rfrench
 * #ifdef'd ZEPHYR stuff
 * 
 * Revision 1.18  87/10/24  00:46:31  wesommer
 * Change acl name to "acl" instead of ".ds_acl"
 * Change default (if no acl file there) to be deny permission.
 * 
 * Revision 1.17  87/10/23  23:49:15  wesommer
 * Implemented access control on meeting creation.
 * 
 * Revision 1.16  87/10/23  21:42:40  wesommer
 * This isn't Multics.
 * 
 * Revision 1.15  87/08/22  22:36:28  rfrench
 * Moved calling location of mtg_znotify -- write_super frees things!
 * 
 * Revision 1.14  87/08/22  18:12:51  rfrench
 * Added Zephyr notifications
 * 
 * Revision 1.13  87/07/16  19:30:46  srz
 * Changed deleted flag to general flags structure on the server.
 * 
 * Revision 1.12  87/04/11  23:48:31  spook
 * Removed unused variable.
 * 
 * Revision 1.11  87/04/11  00:10:00  srz
 * Added RCS junk
 * 
 *
 */
#ifndef lint
static char rcsid_core_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/ocore.c,v 1.1 1993-10-12 05:59:10 probe Exp $";
#endif lint


/* Derived from CORE.PAS 06/21/86 by SRZ */

#include <discuss/types.h>
#include <discuss/dsc_et.h>
#include <discuss/interface.h>
#include "mtg.h"
#include <discuss/tfile.h>
#include "atom.h"
#include <discuss/acl.h>
#include "internal.h"
#include <errno.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <string.h>

#define min(a, b) (a < b ? a : b)
#define NULL 0
extern char *malloc(),*index();
extern char *new_string();
extern long time();
extern off_t lseek();

extern mtg_super super;
extern char *super_chairman;
extern char *super_long_name;
extern char current_mtg [];
extern int u_trn_f,u_control_f,u_acl_f;
extern bool nuclear;
extern afile a_control_f;
extern char rpc_caller [];
extern int errno;
extern int has_privs;
extern int no_nuke;
extern tfile abort_file;
extern dsc_acl *mtg_acl;


/*
 *
 * add_trn () --
 * adds a transaction to the given meeting, either as a reply or an
 * original transaction.  Returns an error code, and the transaction number
 * given to the transaction
 *
 */
add_trn (mtg_name, source_file, subject, reply_trn, result_trn, result)
char *mtg_name;
tfile source_file;
char *subject;
trn_nums reply_trn;		/* trn replying to;  0 if original */
trn_nums *result_trn;		/* trn number given to added trn */
int *result;
{
     add_trn_priv (mtg_name, source_file, subject, NULL, reply_trn, 0,
		   rpc_caller, (date_times) time ((long *)0), 0, 
		   result_trn, result);
}

/*
 *
 * add_trn2 () --
 * adds a transaction to the given meeting, either as a reply or an
 * original transaction.  Returns an error code, and the transaction number
 * given to the transaction.  Also allows a signature for the author.
 *
 */
add_trn2 (mtg_name, source_file, subject, signature, reply_trn, result_trn, result)
char *mtg_name;
tfile source_file;
char *subject, *signature;
trn_nums reply_trn;		/* trn replying to;  0 if original */
trn_nums *result_trn;		/* trn number given to added trn */
int *result;
{
     add_trn_priv (mtg_name, source_file, subject, signature, reply_trn, 0,
		   rpc_caller, (date_times) time ((long *)0), 0,
		   result_trn, result);
}


/* add_trn_priv:  For those who know exactly what they want and who they are */

add_trn_priv (mtg_name, source_file, subject, signature, reply_trn, desired_trn, author, date_entered, flags, result_trn, result)
char *mtg_name;
tfile source_file;
char *subject;
char *signature;
trn_nums reply_trn;		/* trn replying to;  0 if original */
trn_nums desired_trn;		/* trn num desired */
char *author;
date_times date_entered;
int flags;
trn_nums *result_trn;		/* trn number given to added trn */
int *result;
{
     chain_blk reply_cb, cb, spare_cb;
     int tfs,tocopy,i,len;
     char buffer[512],*bptr;
     trn_hdr th;
     

/*   printf ("add_trn:  mtg %s, subject %s, reply %d\n",
	     mtg_name, subject, reply_trn); */
     topen (source_file, "r", result);
     if (*result) return;

     abort_file = source_file;			/* for abort's sake */

     *result = open_mtg (mtg_name);
     if (*result) { core_abort (); return; }

     if (reply_trn) {
	  if (!has_mtg_access('a')) {
	       *result = NO_ACCESS;
	       core_abort (); return;
	  }
     } else {
	  if (!has_mtg_access('w')) {
	       *result = NO_ACCESS;
	       core_abort (); return;
	  }
     }

     if (!no_nuke) {
	  a_control_f = aopen (u_control_f);
	  nuclear = TRUE;
     }

     *result = read_super ();
     if (*result) { core_abort(); return; }

     if (super.date_created == 0) {	/* Meeting has been expunged */
	  write_super();

	  if (!no_nuke) {
	       aclose(a_control_f);
	       nuclear = 0;
	  }

	  *result = open_mtg (mtg_name);
	  if (*result) { core_abort (); return; }

	  if (!no_nuke) {
	       a_control_f = aopen (u_control_f);
	       nuclear = TRUE;
	  }

	  *result = read_super ();
	  if (*result) { core_abort(); return; }
     }	  

     /* check reply_trn */
     if (reply_trn != 0) {
	  *result = read_chain (reply_trn, &reply_cb);
	  if (*result) { core_abort(); return; }
	  if (reply_cb.flags & CB_DELETED) {
	       *result = DELETED_TRN;
	       core_abort (); return;
	  }
     }

     if (desired_trn == 0 || desired_trn <= super.highest)
	  super.highest++;
     else
	  super.highest = desired_trn;
     
     /* Initialize chain block */
     cb.version = CHAIN_BLK_1;
     cb.unique = CHAIN_BLK_UNIQUE;
     cb.current = super.highest;
     cb.prev = super.last;
     cb.next = 0;
     cb.nref = 0;
     cb.chain_fref = 0;
     cb.chain_lref = 0;
     cb.flags = flags & ~CB_DELETED;
     cb.filler = 0;
     cb.trn_addr = fsize (u_trn_f);

     if (reply_trn != 0) {				/* info from pref */
	  cb.trn_chain = reply_cb.trn_chain;
	  read_chain (cb.trn_chain, &spare_cb);
	  cb.pref = spare_cb.chain_lref;
     } else {
	  cb.pref = 0;					/* this is fref */
	  cb.trn_chain = ++super.highest_chain;
     }

     if (write_chain (&cb) != 0)			/* write it out */
	  goto werror;

     /* update fref & lref of chain */
     read_chain (cb.trn_chain, &spare_cb);
     spare_cb.chain_lref = cb.current;			/* update lref */
     if (cb.pref == 0)
	  spare_cb.chain_fref = cb.current;

     if (write_chain (&spare_cb) != 0)
	  goto werror;

     /* update nref of pref */
     if (reply_trn != 0) {
	  read_chain (cb.pref, &spare_cb);
	  spare_cb.nref = cb.current;
	  write_chain (&spare_cb);
     }

     /* update next of prev */
     if (cb.prev != 0) {
	  read_chain (cb.prev, &spare_cb);
	  spare_cb.next = cb.current;
	  write_chain (&spare_cb);
     }

     /* and finish up the super block info */
     super.last = cb.current;
     if (super.first == 0)
	  super.first = cb.current;
     super.date_modified = date_entered;
     super.high_water += sizeof (chain_blk);

     if (signature != NULL && (*signature == '\0' || !strcmp(author, signature)))
	  signature = NULL;			/* Signature is empty */

     /* now write out the transaction to the trn file */
     th.version = TRN_HDR_1;
     th.unique = TRN_HDR_UNIQUE;
     th.current = cb.current;
     th.orig_pref = cb.pref;
     th.date_entered = super.date_modified;
     th.num_lines = 0;			/* count these later */
     th.num_chars = tfsize (source_file);
     th.prev_trn = super.highest_trn_addr;
     super.highest_trn_addr = cb.trn_addr;
     th.subject_len = strlen (subject) + 1;
     th.author_len = strlen (author) + 1;
     if (signature != NULL)
	  th.author_len += strlen (signature) + 1;
     th.subject_addr = cb.trn_addr + sizeof(trn_hdr);
     th.author_addr = th.subject_addr + th.subject_len;
     th.text_addr = th.author_addr + th.author_len;

     lseek (u_trn_f, (long)0, 2);
     if (write (u_trn_f, (char *) &th, sizeof (th)) != sizeof (th)) goto werror;

     if (write (u_trn_f, subject, th.subject_len) != th.subject_len) goto werror;
     if (signature == NULL) {
	  if (write (u_trn_f, author, th.author_len) != th.author_len) goto werror;
     } else {
	  len = strlen(author)+1;
	  if (write (u_trn_f, author, len) != len) goto werror;
	  len = th.author_len - len;
	  if (write (u_trn_f, signature, len) != len) goto werror;
     }

     /* copy transaction from source_file, counting NL's. */
     tfs = th.num_chars;
     while (tfs > 0) {
	  tocopy = min (512, tfs);
	  tocopy = tread (source_file, buffer, tocopy, result);
	  if (*result) { core_abort (); return; }
	  for (bptr = buffer, i = 0; i < tocopy; bptr++,i++)
	       if (*bptr == '\n')
		    th.num_lines++;
	  if (write (u_trn_f, buffer, tocopy) != tocopy) goto werror;
	  tfs -= tocopy;
     }

     tclose(source_file,result);
     abort_file = NULL;

     lseek(u_trn_f, (long)(cb.trn_addr), 0);
     if (write (u_trn_f, (char *) &th, sizeof (trn_hdr)) != sizeof (trn_hdr)) goto werror;	/* update num_lines */

     super.trn_fsize = fsize (u_trn_f);

#ifdef ZEPHYR

     /* Send this out...we want to do this BEFORE calling write_super
      * because things get freed...
      */
     if (!(super.flags & MTG_NOZEPHYR))
	  mtg_znotify(mtg_name, subject, author, signature);
#endif ZEPHYR
     
     /* all done, start winding down */
     write_super();

     if (!no_nuke) {
	  fsync(u_trn_f);
	  aclose(a_control_f);
	  nuclear = 0;
     }

     *result = 0;
     *result_trn = cb.current;
     return;

werror:
     core_abort();
     *result = NO_WRITE;
     return;
}
/*
 *
 * expunge_trn () -- Entry to mark a given transaction as expunged.
 *		     This makes a kosher chain_blk, except there's
 *		     no transaction info associated with this.
 *
 */
expunge_trn(mtg_name, desired_trn, result)
char *mtg_name;
trn_nums desired_trn;
int *result;
{
     chain_blk cb;

     *result = open_mtg (mtg_name);
     if (*result) { core_abort (); return; }

     if (!has_mtg_access('c')) {
	  *result = NO_ACCESS;
	  core_abort (); return;
     }

     if (!no_nuke) {
	  a_control_f = aopen (u_control_f);
	  nuclear = TRUE;
     }

     *result = read_super ();
     if (*result) { core_abort(); return; }

     if (desired_trn == 0 || desired_trn <= super.highest)
	  super.highest++;
     else
	  super.highest = desired_trn;
     
     /* Initialize chain block */
     cb.version = CHAIN_BLK_1;
     cb.unique = CHAIN_BLK_UNIQUE;
     cb.current = super.highest;
     cb.prev = 0;
     cb.next = 0;
     cb.nref = 0;
     cb.chain_fref = 0;
     cb.chain_lref = 0;
     cb.flags |= CB_DELETED;
     cb.filler = 0;
     cb.trn_addr = 0;

     if (write_chain (&cb) != 0)			/* write it out */
	  goto werror;

     super.date_modified = time(0);
     super.high_water += sizeof (chain_blk);

     write_super();

     if (!no_nuke) {
	  aclose(a_control_f);
	  nuclear = 0;
     }

     *result = 0;
     return;

werror:
     core_abort();
     *result = NO_WRITE;
     return;
}

/*
 *
 * get_trn_info () --
 * returns information about the given transaction in info, with an error
 * code as its return argument
 *
 */
get_trn_info (mtg_name, trn, info, result)
char *mtg_name;
trn_nums trn;
trn_info *info;
int *result;
{
     chain_blk cb,spare_cb;
     trn_hdr th;
     char *th_subject,*th_author;

/*   printf ("get_trn_info: mtg %s, trn %d\n",
	     mtg_name, trn);*/

     /* safety -- set info up right */
     info -> version = 0;
     info -> current = 0;
     info -> prev = 0;
     info -> next = 0;
     info -> pref = 0;
     info -> nref = 0;
     info -> fref = 0;
     info -> lref = 0;
     info -> chain_index = 0;
     info -> date_entered = 0;
     info -> num_lines = 0;
     info -> num_chars = 0;
     info -> subject = new_string ("");
     info -> author = new_string ("");


     *result = open_mtg (mtg_name);
     if (*result) return;

     start_read();				/* starting to read */

     *result = read_super ();
     if (*result) { core_abort(); return; }

     *result = read_chain (trn, &cb);
     if (*result) { core_abort(); return; }

     if (cb.trn_addr == 0) {
	  *result = DELETED_TRN;
	  core_abort();
	  return;
     }

     *result = read_chain (cb.trn_chain, &spare_cb);
     if (*result) { core_abort(); return; }

     *result = read_trn (cb.trn_addr, &th, &th_subject, &th_author, NULL);
     if (*result) { core_abort(); return; }

     finish_read();

     if (!has_trn_access(th_author, 'r')) {
	  *result = NO_ACCESS;
	  goto null_info;
     }

     if ((cb.flags & CB_DELETED) && !has_trn_access(th_author, 'd')) {
	  *result = DELETED_TRN;
	  goto null_info;
     }

     info -> version = 1;
     info -> current = cb.current;
     info -> prev = cb.prev;
     info -> next = cb.next;
     info -> pref = cb.pref;
     info -> nref = cb.nref;
     info -> fref = spare_cb.chain_fref;
     info -> lref = spare_cb.chain_lref;
     info -> chain_index = cb.trn_chain;

     info -> date_entered = th.date_entered;
     info -> num_lines = th.num_lines;
     info -> num_chars = th.num_chars;
     free (info -> subject);
     info -> subject = th_subject;
     free (info -> author);
     info -> author = th_author;

     forget_super();

     if (cb.flags & CB_DELETED)
	  *result = DELETED_TRN;
     else
	  *result = 0;
null_info:
     return;
}

/*
 *
 * get_trn_info2 () --
 * returns information about the given transaction in info, with an error
 * code as its return argument.  This call returns expanded information,
 * such as the flags.
 *
 */
get_trn_info2 (mtg_name, trn, info, result)
char *mtg_name;
trn_nums trn;
trn_info2 *info;
int *result;
{
     chain_blk cb,spare_cb;
     trn_hdr th;
     char *th_subject,*th_author;

/*   printf ("get_trn_info: mtg %s, trn %d\n",
	     mtg_name, trn);*/

     /* safety -- set info up right */
     info -> version = 0;
     info -> current = 0;
     info -> prev = 0;
     info -> next = 0;
     info -> pref = 0;
     info -> nref = 0;
     info -> fref = 0;
     info -> lref = 0;
     info -> chain_index = 0;
     info -> date_entered = 0;
     info -> num_lines = 0;
     info -> num_chars = 0;
     info -> subject = new_string ("");
     info -> author = new_string ("");
     info -> flags = 0;

     *result = open_mtg (mtg_name);
     if (*result) return;

     start_read();				/* starting to read */

     *result = read_super ();
     if (*result) { core_abort(); return; }

     *result = read_chain (trn, &cb);
     if (*result) { core_abort(); return; }

     if (cb.trn_addr == 0) {
	  *result = DELETED_TRN;
	  core_abort();
	  return;
     }

     *result = read_chain (cb.trn_chain, &spare_cb);
     if (*result) { core_abort(); return; }

     *result = read_trn (cb.trn_addr, &th, &th_subject, &th_author, NULL);
     if (*result) { core_abort(); return; }

     finish_read();

     if (!has_trn_access(th_author, 'r')) {
	  *result = NO_ACCESS;
	  goto null_info;
     }

     if ((cb.flags & CB_DELETED) && !has_trn_access(th_author, 'd')) {
	  *result = DELETED_TRN;
	  goto null_info;
     }

     info -> version = 1;
     info -> current = cb.current;
     info -> prev = cb.prev;
     info -> next = cb.next;
     info -> pref = cb.pref;
     info -> nref = cb.nref;
     info -> fref = spare_cb.chain_fref;
     info -> lref = spare_cb.chain_lref;
     info -> chain_index = cb.trn_chain;

     info -> date_entered = th.date_entered;
     info -> num_lines = th.num_lines;
     info -> num_chars = th.num_chars;
     free (info -> subject);
     info -> subject = th_subject;
     free (info -> author);
     info -> author = th_author;
     info -> flags = cb.flags;

     forget_super();

     if (cb.flags & CB_DELETED)
	  *result = DELETED_TRN;
     else
	  *result = 0;
null_info:
     return;
}

/*
 *
 * get_trn_info3 () --
 * returns information about the given transaction in info, with an error
 * code as its return argument.  This call returns expanded information,
 * such as the flags and signature.
 *
 */
get_trn_info3 (mtg_name, trn, info, result)
char *mtg_name;
trn_nums trn;
trn_info3 *info;
int *result;
{
     chain_blk cb,spare_cb;
     trn_hdr th;
     char *th_subject,*th_author, *th_signature;

/*   printf ("get_trn_info: mtg %s, trn %d\n",
	     mtg_name, trn);*/

     /* safety -- set info up right */
     info -> version = 0;
     info -> current = 0;
     info -> prev = 0;
     info -> next = 0;
     info -> pref = 0;
     info -> nref = 0;
     info -> fref = 0;
     info -> lref = 0;
     info -> chain_index = 0;
     info -> date_entered = 0;
     info -> num_lines = 0;
     info -> num_chars = 0;
     info -> subject = new_string ("");
     info -> author = new_string ("");
     info -> signature = new_string ("");
     info -> flags = 0;

     *result = open_mtg (mtg_name);
     if (*result) return;

     start_read();				/* starting to read */

     *result = read_super ();
     if (*result) { core_abort(); return; }

     *result = read_chain (trn, &cb);
     if (*result) { core_abort(); return; }

     if (cb.trn_addr == 0) {
	  *result = DELETED_TRN;
	  core_abort();
	  return;
     }

     *result = read_chain (cb.trn_chain, &spare_cb);
     if (*result) { core_abort(); return; }

     *result = read_trn (cb.trn_addr, &th, &th_subject, &th_author, &th_signature);
     if (*result) { core_abort(); return; }

     finish_read();

     if (!has_trn_access(th_author, 'r')) {
	  *result = NO_ACCESS;
	  goto null_info;
     }

     if ((cb.flags & CB_DELETED) && !has_trn_access(th_author, 'd')) {
	  *result = DELETED_TRN;
	  goto null_info;
     }

     info -> version = 1;
     info -> current = cb.current;
     info -> prev = cb.prev;
     info -> next = cb.next;
     info -> pref = cb.pref;
     info -> nref = cb.nref;
     info -> fref = spare_cb.chain_fref;
     info -> lref = spare_cb.chain_lref;
     info -> chain_index = cb.trn_chain;

     info -> date_entered = th.date_entered;
     info -> num_lines = th.num_lines;
     info -> num_chars = th.num_chars;
     free (info -> subject);
     info -> subject = th_subject;
     free (info -> author);
     info -> author = th_author;
     free (info -> signature);
     info -> signature = th_signature;
     info -> flags = cb.flags;

     forget_super();

     if (cb.flags & CB_DELETED)
	  *result = DELETED_TRN;
     else
	  *result = 0;
null_info:
     return;
}


/*
 *
 * set_trn_flags () -- Routine to set the flags (except DELETED) on a
 *		       given transaction.
 *
 */
set_trn_flags (mtg_name, trn, flags, result)
char *mtg_name;
trn_nums trn;
short flags;
int *result;
{
     chain_blk cb;
     trn_hdr th;
     char *th_author;

     *result = open_mtg (mtg_name);
     if (*result) return;

     if (!no_nuke) {
	  a_control_f = aopen (u_control_f);
	  nuclear = TRUE;
     }

     *result = read_super ();
     if (*result) { core_abort(); return; }

     *result = read_chain (trn, &cb);
     if (*result) { core_abort(); return; }

     if ((cb.flags & CB_DELETED) || cb.trn_addr == 0) {
	  *result = DELETED_TRN;
	  core_abort (); return;
     }

     *result = read_trn (cb.trn_addr, &th, (char **)0, &th_author, NULL);
     if (*result) { core_abort(); return; }

     if (!has_trn_access(th_author,'d')) {
	  *result = NO_ACCESS;
	  free(th_author);
	  core_abort(); return;
     }

     free(th_author);

     cb.flags = (cb.flags & CB_DELETED) | (flags & ~CB_DELETED);
     write_chain (&cb);

     forget_super();

     if (!no_nuke) {
	  aclose (a_control_f);
	  nuclear = FALSE;
     }

     *result = 0;
     return;
}



/*
 *
 * delete_trn () --
 * deletes the given transaction from the current meeting.  Returns an
 * error code
 *
 */
delete_trn (mtg_name, trn, result)
char *mtg_name;
trn_nums trn;
int *result;
{
     chain_blk cb, spare_cb;
     char *th_author;
     trn_hdr th;

/*   printf ("delete_trn: mtg %s, trn %d\n",
	     mtg_name, trn);*/

     *result = open_mtg (mtg_name);
     if (*result) return;

     if (!no_nuke) {
	  a_control_f = aopen (u_control_f);
	  nuclear = TRUE;
     }

     *result = read_super ();
     if (*result) { core_abort(); return; }

     *result = read_chain (trn, &cb);
     if (*result) { core_abort(); return; }

     if ((cb.flags & CB_DELETED) || cb.trn_addr == 0) {
	  *result = DELETED_TRN;
	  core_abort (); return;
     }

     *result = read_trn (cb.trn_addr, &th, (char **)0, &th_author, NULL);
     if (*result) { core_abort(); return; }

     if (!has_trn_access(th_author,'d')) {
	  *result = NO_ACCESS;
	  free(th_author);
	  core_abort(); return;
     }

     free(th_author);
     
     cb.flags |= CB_DELETED;
     write_chain (&cb);

     /* update next of prev */
     if (cb.prev != 0) {
	  *result = read_chain (cb.prev, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.next = cb.next;
	  write_chain (&spare_cb);
     }

     /* update prev of next */
     if (cb.next != 0) {
	  *result = read_chain (cb.next, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.prev = cb.prev;
	  write_chain (&spare_cb);
     }

     /* update nref of pref */
     if (cb.pref != 0) {
	  *result = read_chain (cb.pref, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.nref = cb.nref;
	  write_chain (&spare_cb);
     }

     /* update pref of nref */
     if (cb.nref != 0) {
	  *result = read_chain (cb.nref, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.pref = cb.pref;
	  write_chain (&spare_cb);
     }

     /* and update fref & lref of chain */
     if (cb.nref == 0 || cb.pref == 0) {
	  *result = read_chain (cb.trn_chain, &spare_cb);
	  if (*result) { core_abort(); return; }

	  if (cb.nref == 0)
	       spare_cb.chain_lref = cb.pref;
	  if (cb.pref == 0)
	       spare_cb.chain_fref = cb.nref;
	  write_chain (&spare_cb);
     }

     /* and update, first and last of meeting */
     if (cb.prev == 0)
	  super.first = cb.next;
     if (cb.next == 0)
	  super.last = cb.prev;

     write_super ();
     if (!no_nuke) {
	  aclose (a_control_f);
	  nuclear = FALSE;
     }

     *result = 0;
     return;
}

/*
 *
 * retrieve_trn () --
 * retrieves a previously deleted transaction from the current meeting, if
 * possible.  trn must refer to a deleted transaction.  An error code is
 * returned
 *
 */
retrieve_trn (mtg_name, trn, result)
char *mtg_name;
trn_nums trn;
int *result;
{
     chain_blk cb, spare_cb, chain_cb;
     trn_hdr th;
     char *th_author;


/*   printf ("retrieve_trn: mtg %s, trn %d\n",
	     mtg_name, trn);*/


     *result = open_mtg (mtg_name);
     if (*result) return;

     if (!no_nuke) {
	  a_control_f = aopen (u_control_f);
	  nuclear = TRUE;
     }

     *result = read_super ();
     if (*result) { core_abort(); return; }

     *result = read_chain (trn, &cb);
     if (*result) { core_abort(); return; }

     if (!(cb.flags & CB_DELETED)) {
	  *result = TRN_NOT_DELETED;
	  core_abort (); return;
     }

     if (cb.trn_addr == 0) {
	  *result = EXPUNGED_TRN;
	  core_abort (); return;
     }

     /* for paranoia, read transaction */
     *result = read_trn (cb.trn_addr, &th, (char **)0, &th_author, NULL);
     if (*result) { core_abort(); return; }

     if (!has_trn_access(th_author,'d')) {
	  *result = NO_ACCESS;
	  free(th_author);
	  core_abort(); return;
     }

     free(th_author);

     /* now retrieving a transaction is hairier than deleting it, since
	the previous and next, pref and nref could also have been deleted
	since.  Also, intermediate ones could have been retrieved in the
	interim.  So we go to our reference points (fref & lref), and
	start from there.  There are three cases -- we are the new fref,
	we are the new lref, or we are in the middle.  For prev & next,
	we just start decrementing and incrementing until we get a
	non-deleted transaction */
     
     *result = read_chain (cb.trn_chain, &chain_cb);
     if (*result) { core_abort(); return; }

     if (chain_cb.chain_fref > cb.current || chain_cb.chain_fref == 0) /* we are fref */
	  cb.pref = 0;
     else
	  cb.pref = chain_cb.chain_fref;

     if (chain_cb.chain_lref < cb.current)
	  cb.nref = 0;
     else
	  cb.nref = chain_cb.chain_lref;

     /* advance until we get past us */
     while (cb.pref != 0) {
	  *result = read_chain (cb.pref, &spare_cb);
	  if (*result) { core_abort(); return; }

	  if (spare_cb.nref > cb.current || spare_cb.nref == 0)
	       break;
	  cb.pref = spare_cb.nref;
     }

     while (cb.nref != 0) {
	  *result = read_chain (cb.nref, &spare_cb);
	  if (*result) { core_abort(); return; }

	  if (spare_cb.pref < cb.current)
	       break;
	  cb.nref = spare_cb.pref;
     }

     if (super.first > cb.current || super.first == 0) /* we are first */
	  cb.prev = 0;
     else
	  cb.prev = cb.current - 1;

     if (super.last < cb.current)
	  cb.next = 0;
     else
	  cb.next = cb.current + 1;

     while (cb.prev != 0) {
	  *result = read_chain (cb.prev, &spare_cb);
	  if (*result) { core_abort(); return; }

	  if (!(spare_cb.flags & CB_DELETED))
	       break;
	  cb.prev--;
     }
     while (cb.next != 0) {
	  *result = read_chain (cb.next, &spare_cb);
	  if (*result) { core_abort(); return; }

	  if (!(spare_cb.flags & CB_DELETED))
	       break;
	  cb.next++;
     }

     /* invariant -- current_block is all set (except for deleted) */
     cb.flags &= ~(CB_DELETED);
     write_chain (&cb);

     /* update next of prev */
     if (cb.prev != 0) {
	  *result = read_chain (cb.prev, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.next = cb.current;
	  write_chain (&spare_cb);
     }

     /* update prev of next */
     if (cb.next != 0) {
	  *result = read_chain (cb.next, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.prev = cb.current;
	  write_chain (&spare_cb);
     }

     /* update nref of pref */
     if (cb.pref != 0) {
	  *result = read_chain (cb.pref, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.nref = cb.current;
	  write_chain (&spare_cb);
     }

     /* update pref of nref */
     if (cb.nref != 0) {
	  *result = read_chain (cb.nref, &spare_cb);
	  if (*result) { core_abort(); return; }

	  spare_cb.pref = cb.current;
	  write_chain (&spare_cb);
     }

     /* and update fref & lref of chain */
     if (cb.nref == 0 || cb.pref == 0) {
	  *result = read_chain (cb.trn_chain, &spare_cb);
	  if (*result) { core_abort(); return; }

	  if (cb.nref == 0)
	       spare_cb.chain_lref = cb.current;
	  if (cb.pref == 0)
	       spare_cb.chain_fref = cb.current;
	  write_chain (&spare_cb);
     }

     /* and update, first and last of meeting */
     if (cb.prev == 0)
	  super.first = cb.current;
     if (cb.next == 0)
	  super.last = cb.current;

     write_super ();
     if (!no_nuke) {
	  aclose (a_control_f);
	  nuclear = FALSE;
     }

     *result = 0;
     return;
}



/*
 *
 * create_mtg () --
 * Creates a new meeting with the given long_mtg name, where location is the
 * it's place in the hierarchy, and the long_mtg_name is its canonical name.
 * The chairman of the new meeting is the current user.
 *
 */
create_mtg (location, long_mtg_name, public, result)
char *location,*long_mtg_name;
bool public;
int *result;
{
     create_mtg_priv (location, long_mtg_name, public, (date_times) time ((long *)0), rpc_caller, NULL, result);
}

/* create_mtg_priv -- for people who know the chairman and date_created */
create_mtg_priv (location, long_mtg_name, public, date_created, chairman, new_acl, result)
char *location,*long_mtg_name,*chairman;
bool public;
date_times date_created;
dsc_acl *new_acl;
int *result;
{
     char str[256];
     trn_base tb;
     int loclen;

/*   printf("create_mtg: long mtg %s, location %s, public %d\n",
	    long_mtg_name, location, public);*/

     loclen = strlen (location);
     if (location[0] != '/' || loclen == 0 || loclen >= MAXPATHLEN || location [loclen-1] == '/') {
	  *result = BAD_PATH;
	  return;
     }

     if (long_mtg_name [0] == '\0') {
	  *result = BAD_MTG_NAME;
	  return;
     }

     /* First, create meeting directory */
     umask (077);				/* Set access for sure */
     if (mkdir (location, 0700) < 0) {		/* rwx------ */
	  if (errno == EEXIST)
	       *result = DUP_MTG_NAME;
          else
	       *result = BAD_PATH;
	  return;
     }

     /*
      * Then see if we should have access to build it..  Yes, this is
      * a crock, but UNIX doesn't have an easy way to
      * canonicalize a pathname
      */

     *result = 0;
     
     if (!has_privs) {
	  int aclfd;
	  strcpy (str, location);
	  strcat (str, "/../acl");

	  if ((aclfd = open(str, O_RDONLY, 0700)) < 0) {
	       *result = NO_ACCESS;
	  } else {
	       dsc_acl *tmp_acl = acl_read(aclfd);
	       (void) close(aclfd);

	       if (tmp_acl == NULL ||
		   !acl_check(tmp_acl, rpc_caller, "a"))
		       *result = NO_ACCESS;
	       (void) acl_destroy(tmp_acl);
          }
	  if (*result) {
	       (void) rmdir(location); /* we don't care if this fails; */
				       /* we can't do anything about it */
	       return;
	  }
     }
     
     strcpy (str, location);
     strcat (str, "/control");

     if ((u_control_f = open(str, O_RDWR | O_CREAT | O_EXCL, 0700)) < 0) {
	  if (errno == EEXIST)
	       *result = DUP_MTG_NAME;
	  else if (errno == EACCES)
	       *result = NO_ACCESS;
	  else
	       *result = BAD_PATH;
	  return;
     }
     
     strcpy (str, location);
     strcat (str, "/transactions");
     
     if ((u_trn_f = open(str, O_RDWR | O_CREAT | O_EXCL, 0700)) < 0) {
	  if (errno == EEXIST)
	       *result = DUP_MTG_NAME;
	  else if (errno == EACCES)
	       *result = NO_ACCESS;
	  else
	       *result = BAD_PATH;
	  close (u_control_f);
	  return;
     }

     /* Initialize super-block */
     super.version = MTG_SUPER_1;
     super.unique = MTG_SUPER_UNIQUE;
     super.first = 0;
     super.last = 0;
     super.lowest = 1;
     super.highest = 0;
     super.highest_chain = 0;
     super.date_created = super.date_modified = date_created;
     super.long_name_addr = 0;
     super.chairman_addr = 0;
     super.long_name_len = 0;
     super.chairman_len = 0;

     super_long_name = new_string (long_mtg_name);
     super_chairman = new_string (chairman);
     super.flags = public ? MTG_PUBLIC : 0;
     super.chain_start = 1024;
     super.high_water = super.chain_start;
     super.trn_fsize = 0;
     super.highest_trn_addr = 0;

     /* initialize trn_base */
     tb.version = TRN_BASE_1;
     tb.unique = TRN_BASE_UNIQUE;
     tb.date_created = super.date_created;
     tb.public_flag = public;

     /* calculate address & lens of variable length fields */
     tb.long_name_addr = sizeof (tb);
     tb.long_name_len = strlen (super_long_name) + 1;
     tb.chairman_addr = tb.long_name_addr + tb.long_name_len;
     tb.chairman_len = strlen (super_chairman) + 1;
     write (u_trn_f, (char *) &tb, sizeof (tb));	/* trn base */
     write (u_trn_f, super_long_name, tb.long_name_len);
     write (u_trn_f, super_chairman, tb.chairman_len);

     super.trn_fsize = sizeof (tb) + tb.long_name_len + tb.chairman_len;

     write_super();
     
     strcpy (current_mtg, location);			/* it's legal */
     if (mtg_acl != NULL)
	  acl_destroy(mtg_acl);
     if (new_acl == NULL) {
	  mtg_acl = acl_create ();
	  acl_add_access(mtg_acl, chairman, "acdorsw");	/* add chairman */
	  if (public == 1)
	       acl_add_access(mtg_acl, "*", "a  orsw");	/* public mtg */
     } else 
	  mtg_acl = acl_copy(new_acl);


     strcpy (str, location);
     strcat (str, "/acl");

     if ((u_acl_f = open (str, O_RDWR | O_CREAT | O_EXCL, 0700)) < 0) {
	  *result = BAD_PATH;
	  return;
     }
     acl_write (u_acl_f, mtg_acl);
     close (u_acl_f);

     *result = 0;
     return;
}


/*
 *
 * get_mtg_info () --
 * returns information about the given meeting.  Return argument is an
 * error code
 *
 */
get_mtg_info (mtg_name, info, result)
char *mtg_name;
mtg_info *info;
int *result;
{
/*   printf ("get_mtg_info: mtg %s\n",
	     mtg_name);*/

     /* safety -- set strings up right */
     info -> chairman = new_string ("");
     info -> long_name = new_string ("");
     info -> location = new_string (mtg_name);
     info -> access_modes = new_string ("");
     info -> public_flag = TRUE;

     *result = open_mtg (mtg_name);
     if (*result) {
	  if (*result == MTG_MOVED) {
	       char buf[100];
	       int mf;
	       char *cp;

	       strcpy(buf, mtg_name);
	       strcat(buf, "/forward");
	       if ((mf = open(buf, O_RDONLY, 0700)) < 0) {
		    *result = INCONSISTENT;
		    return;
	       }
	       if (read(mf, buf, 100) < 0) {
		    *result = INCONSISTENT;
		    close(mf);
		    return;
	       }
	       close(mf);
	       cp = index(buf, '\n');
	       if (cp == NULL) {
		    *result = INCONSISTENT;
		    return;
	       }
	       *cp = '\0';
	       cp = index(buf, ':');
	       if (cp == NULL) {
		    *result = INCONSISTENT;
		    return;
	       }
	       *cp++ = '\0';
	       free(info -> long_name);
	       info -> long_name = new_string (buf);
	       free(info -> location);
	       info -> location = new_string (cp);
	  }
	  return;
     }

     free(info -> access_modes);
     info -> access_modes = new_string (acl_get_access(mtg_acl, rpc_caller));

     if (!has_mtg_access('s') && !has_mtg_access('r')) {
	  *result = NO_ACCESS;
	  return;
     }

     start_read();				/* starting to read */

     *result = read_super ();
     if (*result) { core_abort(); return; }

     finish_read();

     info -> version = 2;
     free(info -> long_name);
     free(info -> chairman);

     info -> long_name = new_string (super_long_name);
     info -> chairman = new_string (super_chairman);
     info -> first = super.first;
     info -> last = super.last;
     info -> lowest = super.lowest;
     info -> highest = super.highest;
     info -> date_created = super.date_created;
     info -> date_modified = super.date_modified;
     info -> public_flag = (super.flags & MTG_PUBLIC);

     forget_super();

     *result = 0;
     return;
}

/*
 *
 * get_trn () --
 * gets the given transaction, and feeds it through dest_file.  Returns an
 * error code
 *
 */
get_trn (mtg_name, trn, dest_file, result)
char *mtg_name;
trn_nums trn;
tfile dest_file;
int *result;
{
     chain_blk cb;
     trn_hdr th;
     char buffer [512];
     int tocopy, tfs;
     char *th_author;

/*   printf ("get_trn: mtg %s, trn %d\n",
	     mtg_name, trn);*/

     topen (dest_file, "w", result);
     abort_file = dest_file;

     *result = open_mtg (mtg_name);
     if (*result) { core_abort (); return; }

     start_read();				/* starting to read */

     *result = read_super ();
     if (*result) { core_abort(); return; }

     *result = read_chain (trn, &cb);
     if (*result) { core_abort(); return; }

     if (cb.trn_addr == 0) {
	  *result = DELETED_TRN;
	  core_abort();
	  return;
     }

     *result = read_trn (cb.trn_addr, &th, (char **)0, &th_author, NULL);
     if (*result) { core_abort(); return; }

     finish_read();

     if ((cb.flags & CB_DELETED) && !has_trn_access(th_author, 'd')) {

	  *result = DELETED_TRN;
	  free(th_author);
	  core_abort(); return;
     }
     if (!has_trn_access(th_author,'r')) {
	  *result = NO_ACCESS;
	  free(th_author);
	  core_abort(); return;
     }

     (void) free(th_author);

     lseek (u_trn_f, (long)(th.text_addr), 0);
     tfs = th.num_chars;
     while (tfs > 0) {
	  tocopy = min (512, tfs);
	  read (u_trn_f, buffer, tocopy);
	  twrite (dest_file, buffer, tocopy,result);
	  tfs -= tocopy;
     }

     tclose (dest_file,result);
     abort_file = NULL;

     if (cb.flags & CB_DELETED)
	  *result = DELETED_TRN;
     else
	  *result = 0;
     return;
}

/*
 *
 * remove_mtg () --
 * removes the given meeting  -- the physical contents of the meeting
 * are destroyed.
 *
 */
remove_mtg (mtg_name, result)
char *mtg_name;
int *result;
{
     char str[256];


/*   printf ("remove_mtg: mtg %s\n",
	     mtg_name);*/
     
     *result = open_mtg (mtg_name);
     if (*result) return;

     if (!has_mtg_access('c')) {
	  *result = NO_ACCESS;
	  return;
     }
	  
     strcpy (str, mtg_name);
     strcat (str, "/control");

     if (unlink (str) < 0) {
	  if (errno != ENOENT) {
	       *result = CANNOT_REMOVE;
	       return;
	  }
     }

     strcpy (str, mtg_name);
     strcat (str, "/transactions");

     unlink (str);

     strcpy (str, mtg_name);
     strcat (str, "/acl");

     unlink (str);

     *result = 0;
     if (rmdir (mtg_name) < 0)
	  *result = CANNOT_REMOVE;

     *result = read_super();
     super.date_created = 0;
     write_super();

     close (u_trn_f);				/* bombs away */
     close (u_control_f);
     if (u_acl_f)
	  close (u_acl_f);
     acl_destroy(mtg_acl);
     mtg_acl = NULL;
     current_mtg [0] = '\0';

     return;
}

/*
 *
 * updated_mtg () -- Quick procedure to check if the meeting is updated
 *		     with respect to a given time and transaction number.
 *		     An efficient procedure for a common operation -- doesn't
 *		     open the meeting unless it has to.
 *
 */
updated_mtg (mtg_name, date_attended, last, updated, result)
char *mtg_name;
int date_attended, last;
bool *updated;
int *result;
{
     char str[256];
     int mtg_name_len;
     mtg_super mysuper; 
     struct stat sb;
     int uf;

     *updated = 0;
     *result = 0;				/* optimist */

     mtg_name_len = strlen (mtg_name);
     if (mtg_name[0] != '/' || mtg_name_len == 0 || mtg_name_len >= MAXPATHLEN || mtg_name [mtg_name_len-1] == '/') {
	  *result = BAD_PATH;
	  return;
     }

     strcpy (str, mtg_name);
     strcat (str, "/forward");
     if (!stat(str, &sb)) {		/* Show moved meetings as changed */
	  *updated = TRUE;
	  *result = 0;
	  return;
     }

     strcpy (str, mtg_name);
     strcat (str, "/control");

     /* time makes no difference in our books */
     if ((uf = open(str, O_RDWR, 0700)) < 0) {
	  if (errno == ENOENT)
	       *result = NO_SUCH_MTG;
	  else if (errno == EACCES)
	       *result = NO_ACCESS;
	  else
	       *result = BAD_PATH;
	  goto punt;
     }

     /* forget locking (and stuff) for what we're doing */
     lseek (uf, (long)0, 0);
     read (uf, (char *) &mysuper, sizeof (mysuper));
     close(uf);

     *updated = (mysuper.last > last);
     if (mysuper.highest < last)
	  *result = NO_SUCH_TRN;

punt:
     return;
}
