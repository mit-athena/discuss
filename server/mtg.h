/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * mtg.h  -- Include file for things that define the structure of a meeting.
 *
 */

#include <discuss/types.h>

/*
 *
 * A discuss meeting is a directory with various files in it,
 * like the 'trans' file, and the 'control' file.  
 *
 * 'trans' contains the text of the transactions, one after another,
 * and is written in an append-only fashion.  This reduces the chance that
 * actual transactions will become munged, and will make writing utilities
 * to hack meetings (expunging them, salvaging them) very easy.
 *
 * 'control' contains the other parts of the meeting.  This contains the
 * latest information about the meeting itself (the superblock), and
 * how transactions are chained together.
 *
 */

/*
 *
 * mtg_super -- Structure definition for the meeting superblk.  This structure
 * 		sits at the base of the control file.
 *
 *		Short name, long name, and chairman are strings, which we do
 *		not hold in any fixed length field.  They are stored at
 *		location faddr, in the file, and have slen bytes, including
 *		a trailing NULL.  
 * 
 */
typedef struct {
        int version;			/* version of this structure */
	int unique;			/* magic number */
	trn_nums first;			/* first logical trn */
	trn_nums last;			/* last  logical trn */
	trn_nums lowest;		/* lowest  physical trn */
	trn_nums highest;		/* highest physical trn */
	trn_nums highest_chain;		/* highest chain number */
	date_times date_created;	/* when created */
	date_times date_modified;	/* when modified */
	
	faddr long_name_addr;		/* location of long name string */
	faddr chairman_addr;		/* location of chairman string */
	slen long_name_len;		/* len of long name */
	slen chairman_len;		/* len of chairman */
	bool public_flag;		/* meeting is public */
	faddr chain_start;		/* starting address for chain structure */
	faddr high_water;		/* next byte to be used in control file */
	faddr trn_fsize;		/* next byte to be used in trn file */
	faddr highest_trn_addr;		/* address of highest trn addr */
} mtg_super;

/* version number */
#define MTG_SUPER_1 1

/* unique number */
#define MTG_SUPER_UNIQUE 100866


/*
 *
 * chain_blk -- Basic component of the chain structure, which is kept in the
 * 		control file.  Since it is a fixed length structure,
 *		the chain_blk's are just an array based at 
 *		mtg_super.chain_start, extending to the end of the file.
 *
 *		The information in this structure describes two different
 *		entities, which are merged for convenience's sake.  The
 *		first half describes the chaining information for the
 *		transaction numbered 'current'.  The second half describes
 *		the chain numbered 'current'.  We can do this because
 *		we will never have more chains than transactions.
 *
 */
typedef struct {
        int version;				/* version number */
	int unique;				/* magic number */
	trn_nums current;			/* this trn num */
	trn_nums prev;				/* previous non-deleted trn */
	trn_nums next;				/* next trn */
	trn_nums pref;				/* pref trn */
	trn_nums nref;				/* nref trn */
	int trn_chain;				/* which chain trn is in */
	faddr trn_addr;				/* location of trn */
	short flags;				/* transaction deleted, etc */
	bool filler;				/* filler -- must be zero */

	/* the rest of this information describes the chain numbered current */
	trn_nums chain_fref;			/* fref of chain */
	trn_nums chain_lref;			/* lref of chain */
} chain_blk;

/* version & magic */
#define CHAIN_BLK_1 1
#define CHAIN_BLK_UNIQUE 102966

/* flags for transactions */
#define CB_DELETED 1

/*
 *
 * trn_base --  base of transaction file.  This structure records
 * 		historical information about the creation of this meeting.
 *		This is kept just in case something gets destroyed.
 *
 */

typedef struct {
        int version;			/* version of this structure */
	int unique;			/* magic number */
	date_times date_created;	/* when created */
	faddr long_name_addr;		/* location of long name string */
	faddr chairman_addr;		/* location of chairman string */
	slen long_name_len;		/* len of long name */
	slen chairman_len;		/* len of chairman */
	bool public_flag;		/* meeting is public */
} trn_base;

#define TRN_BASE_1 1
#define TRN_BASE_UNIQUE 070476

/*
 *
 *  trn_hdr --  defines the header of a transaction.  This holds everything
 *		but the chaining information, except it remembers what pref
 *		was (so that chains can be reconstructed).
 *
 */
typedef struct {
        int version;				/* version of this struct */
	int unique;				/* magic number */
	trn_nums current;			/* this transaction number */
	trn_nums orig_pref;			/* original pref trn */
	date_times date_entered;		/* date/time trn entered */
	int num_lines;				/* # lines in trn */
	int num_chars;				/* # chars in trn */
	faddr prev_trn;				/* addr of prev */
	faddr subject_addr;			/* address of subject */
	faddr author_addr;			/* addr of author & signature*/
	faddr text_addr;			/* address of trn text */
	slen subject_len;			/* subject len (incl NULL) */
	slen author_len;			/* author + signature len */
} trn_hdr;

#define TRN_HDR_1 1
#define TRN_HDR_UNIQUE 102463
