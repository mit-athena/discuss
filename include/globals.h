/*
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/globals.h,v 1.5 1986-12-07 03:00:04 rfrench Exp $
 * $Locker:  $
 *
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
     char *mtg_uid;				/* meeting uid */
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

extern int	errno;		/* lusing UNIX method to pass error values */

#define	CURRENT_VERSION	"1.0"


/* Subroutine declarations */

extern void	get_trn_map();
extern selection_list *trn_select();

