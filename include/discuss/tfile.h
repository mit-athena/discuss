/*
 *
 * tfile.h  -- Definition for internal 'tfile's.
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/discuss/tfile.h,v 1.2 1991-09-04 11:33:58 lwvanels Exp $
 * $Log: not supported by cvs2svn $
 */

struct _tfile {
     int (*proc)();				/* file proc */
     int size;					/* file size */
     char *infop;				/* pointer to misc info */
     int info;
};

typedef struct _tfile *tfile;

#define TFOPEN 1
#define TFCLOSE 2
#define TFREAD 3
#define TFWRITE 4
#define TFDESTROY 5
#define TFCONTROL 6

/* TF Control orders (start high just for fun) */

#define TFC_FORCE_NL 15

tfile tcreate();
