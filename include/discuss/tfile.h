/*
 *
 * tfile.h  -- Definition for internal 'tfile's.
 *
 * $Id: tfile.h,v 1.3 1999-01-22 23:09:52 ghudson Exp $
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
