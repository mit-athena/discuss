/*
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/globals.h,v 1.2 1986-09-22 06:19:58 spook Exp $
 * $Locker:  $
 *
 */

/* Variables */

extern trn_nums	cur_trans;	/* which is current transaction */
extern char	*cur_mtg;	/* current meeting identifier */
extern mtg_info	m_info;		/* current meeting info */
extern char	*buffer;
extern int	time_now, time_sixmonthsago;
extern int	dsc_sci_idx;	/* sci_idx for global usage */
extern int	*chosen_trn_map; /* which trns we want to see */
extern int	map_size;	/* size of chosen_trn_map */
extern char	*temp_file;	/* generic temporary file... */

extern int	errno;		/* lusing UNIX method to pass error values */

#define	CURRENT_VERSION	"1.0"

/* Typedefs */

typedef struct _sle {
	int low;
	int high;
	struct _sle *next;
} selection_list;

/* Subroutine declarations */

extern void	get_trn_map();
extern selection_list *trn_select();

