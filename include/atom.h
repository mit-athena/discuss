/*
 *
 * atom.h -- 	Include file for external atomic file declarations, such
 * 	     	as what an afile is.
 *
 */
typedef struct {
     int desc;					/* UNIX file descriptor */
     char *dir_list;				/* list of blocks */
     int dirty_blks;				/* number of dirty blocks */
     int file_size;				/* original size of file */
} *afile;

afile aopen();
