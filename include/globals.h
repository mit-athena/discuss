/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/globals.h,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/globals.h,v 1.14 1989-01-05 00:08:54 raeburn Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *	Declarations of global variables for discuss.
 */

#include <discuss/dsname.h>

/* Typedefs */

typedef struct _sle {
	int low;
	int high;
	int flags;
	struct _sle *next;
} selection_list;

/* flags */
#define	flag_ONLY_INITIAL	1
#define	flag_ONLY_DELETED	2
#define	flag_INCLUDE_DELETED	4

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

