/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: globals.h,v 1.22 2003-03-20 06:59:38 ghudson Exp $
 *
 *	Declarations of global variables for discuss.
 */

#include <discuss/dsname.h>
#include "discuss_err.h"
#include <errno.h>

/* Typedefs */

typedef struct _sle {
	int low;
	int high;
	int flags;
	struct _sle *next;
} selection_list;

/* selection flags */
#define flag_AREF		8

/* filtering flags */
#define filter_INCLUDE_DELETED 1
#define filter_ONLY_DELETED    2
#define filter_ONLY_INITIAL    4
#define filter_ONLY_TERMINAL   16
#define filter_FLAG_SET        32
#define filter_FLAG_RESET      64

struct _dsc_pub {
     trn_nums current;
     trn_nums highest_seen;
     bool attending;
     char *host;
     char *path;
     char *mtg_name;				/* meeting name (user) */
     name_blk nb;
     mtg_info m_info;
};

/* Variables */

extern struct _dsc_pub dsc_public;
extern char	dsc_version[];
extern char	*buffer;
extern int	*chosen_trn_map; /* which trns we want to see */
extern int	map_size;	/* size of chosen_trn_map */
extern char	*temp_file;	/* generic temporary file... */
extern int	sci_idx;

extern int	interrupt;

extern int	errno;		/* lusing UNIX method to pass error values */

extern bool 	use_editor;	/* Should we snarf input from stdin, or 
				 * use an editor instead? */
extern char	*editor_path;	/* Pathname of editor to use */

extern	char	*user_id;	/* user.instance@realm identifier for user */

/* Subroutine declarations */

extern void	get_trn_map();
extern selection_list *trn_select();

#if defined(__GNUC__)
#undef alloca
#define alloca(x) __builtin_alloca(x)
#endif
