/*
 *
 * dsname.h -- Include file for discuss meeting routines.
 *
 */

typedef struct {
	char *hostname;
	char *pathname;
	char *user_id;
	char **aliases;
	char *spare;		/* "reserved for future expansion" */
	int status;		/* 1=changed (ckm) */
	int date_attended;
	int last;
} name_blk;

typedef struct {
	char *hostname;
	char *pathname;
	char *user_id;
	char *alias_list;
	char *spare;
	int status;
	int date_attended;
	int last;
} server_name_blk;

#define	DSC_ST_CHANGED	1
