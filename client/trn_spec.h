/* Transaction Generator */
#define TRSPEC_REVERSE	0x0002
#define TRSPEC_DEL	0x0004
#define TRSPEC_UNSEEN	0x0010 

/* Transaction entry flags */
#define TRSPEC_REM	0x0001	/* Remove this entry from the chain */
#define TRSPEC_UNTIL	0x0008

/* Types */

#define TRSPEC_NUM	0x0000	/* Specified trn. number */
#define TRSPEC_CUR	0x0100
#define TRSPEC_NEXT	0x0200
#define TRSPEC_PREV	0x0300
#define TRSPEC_NREF	0x0400
#define TRSPEC_PREF	0x0500
#define TRSPEC_FIRST	0x0600
#define TRSPEC_LAST	0x0700
#define TRSPEC_FREF	0x0800
#define TRSPEC_LREF	0x0900
#define TRSPEC_NCHAIN	0x0a00
#define TRSPEC_PCHAIN	0x0b00
#define TRSPEC_FUNS	0x0c00
#define TRSPEC_KINDS	0xff00
#define TRSPEC_REVBIT	0x0100	    /* Toggle this bit to reverse */
				    /* direction */

typedef struct _te {
	struct _te *te_prev;
	struct _te *te_next;
	unsigned int flags;
	int num;
} trnspec_entry;

typedef struct {
	trnspec_entry	*te;	/* First */
	trnspec_entry	*te_last;
	trn_info	tinfo;
	mtg_info	*minfo; /* Owned by caller. */
	name_blk	*nbp;
	unsigned int	flags;
	int		current;
	int		highest_seen;
} trans_gen;
