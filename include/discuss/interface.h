     /********************************************************************
      *                                                                  *
      *         Copyright (c) 1985 by                                    *
      *            Lewis Makepeace and Stan Zanarotti                    *
      *                                                                  *
      *                                                                  *
      *         All rights reserved.                                     *
      *                                                                  *
      ********************************************************************/

/*
 *
 * interface.h -- Include file for core interface.  This file contains
 * definitions for structures and routines that will be used to communicate
 * with the core discuss routines.  The exact protocol between the two 
 * has not been defined, although something ala RPC should be investiaged
 *
 */

#include "types.h"

/* this include maybe should be split apart eventually. */

/* Derived from CORE.PAS 06/21/86 by SRZ */


/*
 *
 *   trn_info: This is the information about a transaction that is
 *   returned by core routines, except the transaction contents itself.
 *   This is for list commands, etc.
 *
 */

typedef struct {
	int version;		 /* version of this structure */
	trn_nums current;	 /* trans # of this transaction */
	trn_nums prev;		 /* trans # of previous non-deleted trans */
	trn_nums next;		 /* etc, etc. */
	trn_nums pref;
	trn_nums nref;
	trn_nums fref;
	trn_nums lref;

 	int chain_index;	 /* index of transaction in chain. Fref is 1 */
	date_times date_entered; /* date/time transaction was entered */
	int num_lines;		 /* # lines in transaction */
	int num_chars;		 /* # chars in transaction */
	char *subject;		 /* subject of transaction */
	char *author;		 /* author of transaction */
} trn_info;

/*
 *
 * mtg_info:  This is information about a meeting that is returned
 *   by core routines, giving information about the meeting itself.
 *
 */

typedef struct {
	int version;			/* version of this structure */
	pathnames location;		/* location of actual meeting */
	mtg_names long_name;		/* long name of meeting */
	char *chairman;			/* #1 chairman of meeting */
	trn_nums first,last;		/* first & last non-dl trns of mtg */
	trn_nums lowest,highest;	/* first & last maybe-dl trns */

	date_times date_created;	/* date meeting created */
	date_times date_modified;	/* last modification of meeting */

	bool public_flag;		/* if mtg is public */
	char *access_modes;		/* Access modes we have to this mtg. */
} mtg_info;


/*
 *
 * Routines for interface.  They are defined below in C syntax, although
 * commented out
 *
 */


/*

void add_trn (mtg_name, source_file, subject, reply_trn, result_trn, result)
char *mtg_name;
tfile source_file;
char *subject;
trn_nums reply_trn;		* trn replying to;  0 if original *
trn_nums *result_trn;		* trn number given to added trn *
code *result;
{}

 *
 *
 * adds a transaction to the current meeting, either as a reply or an
 * original transaction.  Returns an error code, and the transaction number
 * given to the transaction
 *
 *


void get_trn_info (mtg_name, trn, info, result)
char *mtg_name;
trn_nums trn;
trn_info *info;
code *result;
{}

 *
 *
 * returns information about the given transaction in info, with an error
 * code as its return argument
 *
 *


void delete_trn (mtg_name, trn, result)
char *mtg_name;
trn_nums trn;
code *result;
{}

 *
 *
 * deletes the given transaction from the current meeting.  Returns an
 * error code
 *
 *


void retrieve_trn (mtg_name, trn, result)
char *mtg_name;
trn_nums trn;
code *result;
{}

 *
 *
 * retrieves a previously deleted transaction from the current meeting, if
 * possible.  trn must refer to a deleted transaction.  An error code is
 * returned
 *
 *


void create_mtg (location, long_mtg_name, public, hidden, result)
char *location,*long_mtg_name;
bool public,hidden;
code *result;
{}

 *
 *
 * Creates a new meeting with the given long_mtg name, where location is the
 * it's place in the hierarchy, and the long_mtg_name is its canonical name.
 * The chairman of the new meeting is the current user.
 *
 *


void get_mtg_info (mtg_name, info, result)
char *mtg_name;
mtg_info *info;
code *result;
{}

 *
 *
 * returns information about the given meeting.  Return argument is an
 * error code
 *
 *


void start_mtg_info ()
{}


void get_trn (mtg_name, trn, dest_file, result)
char *mtg_name;
trn_nums trn;
tfile dest_file;
code *result;
{}

 *
 *
 * gets the given transaction, and feeds it through dest_file.  Returns an
 * error code
 *
 *


void remove_mtg (mtg_name, result)
char *mtg_name;
code *result;
{}

 *
 *
 * removes the given meeting from the meeting list.  This should
 * probably go ahead and physically remove the contents of the meeting
 * but that's not obvious if we have the access to do that.
 *
 *


/* end of routine definitions */
