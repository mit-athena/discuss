/*
 *
 * dsname.h -- Include file for discuss meeting routines.
 *
 */

#define NB_USER_SZ 40
#define NB_MTG_NAME_SZ 32
#define NB_UNIQUE_SZ  140

typedef struct {
	char *user_id;
	char *mtg_name;
	char *unique_id;
	int status;		/* 1=invisible */
	int date_attended;
	int last;
} name_blk;


