/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * atom.c --    File to support the idea of 'atomic' files in C.  These are
 *	        files that support the ideas of coordination and failure
 *		atomicity.  Coordination is handled by using flock on the
 *		file to get an exclusive lock (supposedly other accessors
 *		will honor this).  Failure is handled currently by keeping
 *		all the changes in memory, and writing it out when it is
 *		closed.  If the system crashes before this happens, the old
 *		version will still be around.  If the system crashes after
 *		this happens, the new version will be around.  
 *
 *		This is banking on the fact that system crashes during the
 *		atomic close is not likely.  If this is an incorrect
 *		assumption, then this can be recoded to update a new version
 *		of the file, and 'mv' it over.  Or we could play with shadow
 *		blocks or something else.
 *
 */


#include "../include/atom.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifdef SOLARIS
#include <fcntl.h>
#endif
#define NULL 0
#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)


/* chunk size of atomic blocks */
#define ABLOCKSIZE 512
#define ABLKSINDIR 24

typedef unsigned short block_num;

struct dir_blk {
     struct dir_blk *next;
     short used;
     char *bptr [ABLKSINDIR];
     block_num bnum [ABLKSINDIR];
};


char *malloc(),*calloc();
char *find_block();
off_t lseek();

static int maxdirty = 0;				/* meter: maximum dirty blks */

/*
 *
 * aopen --     opens up an atomic file, returning an AFILE back to the caller.
 *		The caller gives a UNIX file number that he wants to go
 *		nuclear.
 *
 */
afile aopen (d)
int d;
{
     afile af;
     struct stat buf;
#ifdef SOLARIS
     struct flock lock;
#endif

#ifdef SOLARIS
    lock.l_type = F_WRLCK;
    lock.l_start = 0;
    lock.l_whence = 0;
    lock.l_len = 0;
    if (fcntl(d, F_SETLK, &lock)) 
      	  return (NULL);
#else
     if (flock(d, LOCK_EX) < 0)
	  return (NULL);
#endif
     if (fstat (d, &buf) < 0)
	  goto punt;

     af = (afile) malloc (sizeof (*af));
     if (af == NULL)
	  goto punt;				/* no memory, ack! */

     af -> desc = d;
     af -> dir_list = NULL;
     af -> dirty_blks = 0;			/* no dirty blocks yet */
     af -> file_size = buf.st_size;

     return (af);

punt:
#ifdef SOLARIS
     lock.l_type = F_UNLCK;
     lock.l_start = 0;
      lock.l_whence = 0;
      lock.l_len = 0;
     fcntl(d, F_SETLK, &lock);
#else
     flock(d, LOCK_UN);
#endif
     return (NULL);
}

/*
 *
 * aread -- 	Atomic read.  Reads from an atomic file.  Pos is the place to
 *		lseek to.   This checks to see if the desired data is in
 *		memory, and if so, reads it.  Otherwise it simply reads from
 *		the file.
 *
 */
aread(af,buf,nbytes,pos)
afile af;
int nbytes, pos;
char *buf;
{
     int hi, numleft, toread, offset;
     char *bptr,*dest_ptr;
     block_num bstart,bend,bn;
     int result;

     hi = pos + nbytes - 1;				/* high end of range */
     bstart = pos / ABLOCKSIZE;
     bend = hi / ABLOCKSIZE;
     offset = pos % ABLOCKSIZE;				/* offset of blk we're playing with */

     numleft = nbytes;
     dest_ptr = buf;					/* caller's buffer */

     for (bn = bstart; bn <= bend; bn++) {
	  toread = min (numleft, ABLOCKSIZE - offset);
	  if ((bptr = find_block (af, bn)) == NULL) {	/* not there, read file */
	       lseek (af -> desc, (long)(bn * ABLOCKSIZE + offset), 0);
	       result = read (af -> desc, dest_ptr, toread);
	       if (result != toread)
		    goto read_error;
	  } else {
#ifdef POSIX
	       memmove (dest_ptr, bptr + offset,  toread);
#else
	       bcopy (bptr + offset, dest_ptr, toread);
#endif
	  }
	  dest_ptr += toread;
	  numleft -= toread;
	  offset = 0;					/* start from blk beginning next time */
     }

     return (nbytes);

read_error:
     return (result);
}

/*
 *
 * awrite () -- Routine to handle atomic writing of the file.  This
 *		hairy beast allocates blocks when needed.
 *
 */
awrite(af,buf,nbytes,pos)
afile af;
int nbytes, pos;
char *buf;
{
     int hi, numleft, towrite, offset;
     char *bptr,*src_ptr;
     block_num bstart,bend,bn;
     int result;
     
     hi = pos + nbytes - 1;				/* high end of range */
     bstart = pos / ABLOCKSIZE;
     bend = hi / ABLOCKSIZE;
     offset = pos % ABLOCKSIZE;
     
     numleft = nbytes;
     src_ptr = buf;					/* caller's buffer */
     for (bn = bstart; bn <= bend; bn++) {
	  towrite = min (ABLOCKSIZE - offset, numleft);
	  if ((bptr = find_block (af, bn)) == NULL) {	/* not there, make new one */
	       bptr = calloc (1, ABLOCKSIZE);
	       lseek (af -> desc, (long)(bn * ABLOCKSIZE), 0);
	       result = read (af -> desc, bptr, ABLOCKSIZE);
	       if (result < 0)
		    goto write_error;
	       /* write block back out, thus reserving quota */
	       lseek (af -> desc, (long)(bn * ABLOCKSIZE), 0);
	       result = write (af -> desc, bptr, ABLOCKSIZE);
	       if (result < 0)
		    goto write_error;

	       add_block (af, bn, bptr);
	  }
#ifdef POSIX
	  memmove (bptr+offset, src_ptr, towrite);
#else
	  bcopy (src_ptr, bptr+offset, towrite);
#endif
	  src_ptr += towrite;
	  numleft -= towrite;
	  offset = 0;					/* after first, no offset */
     }
     return (nbytes);

write_error:
     return (result);
}

/*
 *
 * aclose --    Atomic closes the file.  This is where the action is.  Our
 *		changes are in memory, and we have to write it all back out.
 *
 *
 */
aclose(af)
afile af;
{
     struct dir_blk *db,*olddb;
     register i;
#ifdef SOLARIS
    struct flock lock;
#endif

     /* loop thru dir blocks, writing all blocks */
     for (db = (struct dir_blk *) af -> dir_list; db != NULL;) {
	  for (i = 0; i < db -> used; i++) {
	       lseek (af -> desc, (long)(db -> bnum[i] * ABLOCKSIZE), 0);
	       write (af -> desc, db -> bptr[i], ABLOCKSIZE);
	       free (db -> bptr[i]);
	       db -> bptr[i] = 0;
	  }
	  olddb = db;
	  db = db -> next;
	  free((char *)olddb);
     }

     fsync(af -> desc);				/* tell kernel to get a move on */
#ifdef SOLARIS
     lock.l_type = F_UNLCK;
     lock.l_start = 0;
     lock.l_whence = 0;
     lock.l_len = 0;
     fcntl(af -> desc, F_SETLK, &lock);
#else
     flock(af -> desc, LOCK_UN);		/* and to let others at it */
#endif
     af -> dir_list = NULL;
     maxdirty = max(maxdirty, af -> dirty_blks);
     af -> desc = -1;				/* to prevent reuse */
     (void) free ((char *)af);
}

/*
 *
 * aabort --    Routine to close an atomic file, without writing the changes
 *		out.
 *
 */

aabort(af)
afile af;
{
     struct dir_blk *db,*olddb;
     register i;
#ifdef SOLARIS
    struct flock lock;
#endif


     /* loop thru dir blocks, freeing all blocks */
     for (db = (struct dir_blk *) af -> dir_list; db != NULL;) {
	  for (i = 0; i < db -> used; i++) {
		  (void) free (db -> bptr[i]);
		  db -> bptr[i] = 0;
	  }
	  olddb = db;
	  db = db -> next;
	  (void) free((char *)olddb);
     }

     ftruncate(af -> desc, (long)(af -> file_size));
#ifdef SOLARIS
     lock.l_type = F_UNLCK;
     lock.l_start = 0;
     lock.l_whence = 0;
     lock.l_len = 0;
     fcntl(af -> desc, F_SETLK, &lock);
#else
     flock(af -> desc, LOCK_UN);

#endif
     af -> dir_list = NULL;
     maxdirty = max(maxdirty, af -> dirty_blks);
     af -> desc = -1;				/* to prevent reuse */
     (void) free ((char *)af);
}


/*
 *
 * find_block -- Checks the block list for the given block, and returns
 *		 a pointer to the block.
 *
 */

char *find_block(af, bnum)
afile af;
block_num bnum;
{
     int i;
     struct dir_blk *db;

     if (af -> dir_list != NULL) {			/* anything there? */
	  for (db = (struct dir_blk *) af -> dir_list; db != NULL; db = db -> next) {
	       for (i = 0; i < db -> used; i++)
		    if (bnum == db -> bnum [i])		/* found it */
			 return (db -> bptr [i]);
	  }
     }

     return (NULL);
}

/*
 *
 * add_block -- Add a new block to the dir structure.
 *
 */
add_block (af, bnum, bptr)
afile af;
block_num bnum;
char *bptr;
{
     struct dir_blk *db;

     if (af -> dir_list == NULL) {			/* create dir list */
          af -> dir_list = calloc (1, sizeof (struct dir_blk));
	  /* calloc initializes everything we need... */
     }

     db = (struct dir_blk *) af -> dir_list;
     if (db -> used >= ABLKSINDIR) {			/* no space, make new */
	  db = (struct dir_blk *) calloc (1, sizeof (struct dir_blk));
	  db -> next = (struct dir_blk *) af -> dir_list;
	  af -> dir_list = (char *) db;
	  /* fall thru to add into db (which has space now) */
     }

     db -> bptr [db -> used] = bptr;
     db -> bnum [db -> used] = bnum;
     db -> used++;

     af -> dirty_blks++;

     return;
}

