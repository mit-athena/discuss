/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
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
