/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */

/*
 * edsc.h --- include file for the emacs discuss back-end program
 */

#include <discuss/discuss.h>

extern char *user_id;
extern tfile stdout_tf;
extern int bit_bucket();
extern int errno;
extern char *do_quote();
tfile unix_tfile();
extern int cache_activated, cache_working;
extern int use_vectors;

extern char *malloc();

extern int	do_quit(), do_gmi(), do_gti(), do_gcm(), do_gml(), do_gt();
extern int	do_gtf(), do_grt(), do_grtn(), do_ss(), do_at(), do_nut();
extern int	do_sfl(), do_am(), do_dm(), do_gpv();
extern int	do_pacl(), do_sacl(), do_dacl();
extern int	do_dt(), do_rt(), do_ls();

/* selection flags */
#define flag_AREF		8

/* filtering flags */
#define filter_INCLUDE_DELETED 1
#define filter_ONLY_DELETED    2
#define filter_ONLY_INITIAL    4
#define filter_ONLY_TERMINAL   16
#define filter_FLAG_SET        32
#define filter_FLAG_RESET      64

/*
 * Here is the cache information.....
 */

/*
 * The caching is only enabled if EDSC_CACHE is defined....
 */
#define EDSC_CACHE

#ifdef EDSC_CACHE
#define CACHE_DIR	"/tmp"
#define CACHE_FN_SIZE	80
#define CACHE_STRATEGY_SIZE	256
#define CACHE_DEFAULT_SIZE	50

#define CACHE_FLG_INFO	1
#define CACHE_FLG_TEXT	2

struct cache_meeting {
	int	ref_count;
	name_blk	nb;
	mtg_info	m_info;
	int		meeting_code; /* Error code when fetching */
				      /* meeting info */
	struct cache_meeting	*next;
	struct cache_meeting	*prev;
};

struct cache_info {
	trn_nums trn_num;
	struct cache_meeting	*meeting;
	int	flags;
	char	filename[CACHE_FN_SIZE];
	trn_info3 t_info;
	int	info_code;	/* Error code when fetching info */
	int	text_code;	/* Error code when fetch the text */
	struct cache_info      	*next;	/* Used to keep LRU list */
					/* Also used for empty list */
	struct cache_info	*prev;
};

/*
 * The cache work instructions are divided into two fields:
 *
 * The upper 4 bits define the opcode, the lower 4 bites define the
 * direction, if any.
 *
 * OPCODES:
 * 	STOP --- Stop doing any cache work.
 * 	GET_CUR --- Get the current transaction
 * 	FETCH_CUR --- Fetch the article one away in the indicated direction
 * 			from the current article.
 * 	SET_PTR --- Set the pointer to the current article
 * 	MFETCH_PTR --- Move the pointer one in the indicated direction and
 * 			fetch that article
 */

typedef char	cache_dir;

#define CACHE_OP_MASK		0xF0
#define CACHE_OP_STOP		(0 << 4)
#define CACHE_OP_GET_CUR	(1 << 4)
#define CACHE_OP_FETCH_CUR	(2 << 4)
#define CACHE_OP_SET_PTR	(3 << 4)
#define CACHE_OP_MFETCH_PTR	(4 << 4)

#define CACHE_DIR_MASK		0x0F
#define CACHE_DIR_CURRENT	0
#define CACHE_DIR_NEXT		1
#define CACHE_DIR_PREV		2
#define CACHE_DIR_NREF		3
#define CACHE_DIR_PREF		4
#define CACHE_DIR_FREF		5
#define CACHE_DIR_LREF		6


/*
 * Function declarations
 */
	
struct cache_info *cache_empty(), *cache_search();
struct cache_meeting *get_cache_meeting(), *search_cache_meetings();

void cache_get_transaction_text(), cache_flush(), free_cache_entry();
void free_cache_meeting(), do_cache_work(), cache_init();

extern int	do_scd(), do_gtfc(), do_it(), do_itn(), do_im();
extern int	cache_pc;
extern struct cache_info *cache_current;
extern cache_dir		cache_current_direction;
	
	
#endif /* EDSC_CACHE */

