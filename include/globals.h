/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/globals.h,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/globals.h,v 1.11 1987-04-11 00:54:40 spook Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *	Declarations of global variables for discuss.
 */

#include "dsname.h"

/* Typedefs */

typedef struct _sle {
	int low;
	int high;
	struct _sle *next;
} selection_list;

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
extern char	*buffer;
extern int	time_now, time_sixmonthsago;
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

#define	CURRENT_VERSION	"1.1"


/* Subroutine declarations */

extern void	get_trn_map();
extern selection_list *trn_select();

