/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v 1.7 1993-04-28 11:34:09 miki Exp $
 *	$Locker:  $
 *
 */

#ifndef lint
static char rcsid_announce_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v 1.7 1993-04-28 11:34:09 miki Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#ifdef SOLARIS
#include <fcntl.h>
#endif
#include <discuss/tfile.h>
#include <discuss/interface.h>
#include <discuss/dsname.h>
#include <discuss/dsc_et.h>

#define min(x,y) ((x)<(y)?(x):(y))

extern tfile unix_tfile();

dsc_announce_mtg (nbpsrc, nbpdest, public, tf, txn_no, code_ptr)
name_blk *nbpsrc, *nbpdest;
int public;
tfile tf;
int *txn_no;
int *code_ptr;
{
	char temp_file[64];
	char buffer[512],subject[100];
	int fd,tfs,tocopy,mycode;
	FILE *fp;
	tfile tf2;
	mtg_info my_minfo;

	*code_ptr = 0;
        fp = NULL;
	tf2 = NULL;

	dsc_get_mtg_info(nbpsrc,
			 &my_minfo, code_ptr);
	if (*code_ptr != 0)
	     return;

	(void) sprintf(temp_file,"/tmp/mtgz%d.%d",getuid(),getpid());
	(void) unlink(temp_file);

	fp = fopen(temp_file,"w");
	if (!fp) {
		*code_ptr = CANT_WRITE_TEMP;
		goto punt;
	}
	fprintf(fp,"  Meeting Name:  %s\n", my_minfo.long_name);
	fprintf(fp,"  Host:          %s\n", nbpsrc->hostname);
	fprintf(fp,"  Pathname:      %s\n", nbpsrc->pathname);
	fprintf(fp,"  Participation: %s\n", public?"Public":"Private");
	fprintf(fp,"\n");
	fclose(fp);
	fp = NULL;

	fd = open(temp_file,O_APPEND|O_RDWR,0);
	if (fd < 0) {
		*code_ptr = CANT_WRITE_TEMP;
		goto punt;
	}
	tf2 = unix_tfile(fd);
	tfs = tfsize(tf);
	while (tfs > 0) {
		tocopy = min (512, tfs);
		tocopy = tread (tf, buffer, tocopy, code_ptr);
		if (*code_ptr)
			goto punt;
		twrite (tf2, buffer, tocopy, code_ptr);
		if (*code_ptr)
			goto punt;
		tfs -= tocopy;
	}
	(void) tclose(tf2, code_ptr);
	tf2 = NULL;
	*code_ptr = 0;

	fd = open(temp_file,O_RDONLY,0);
	if (fd < 0) {
		*code_ptr = CANT_WRITE_TEMP;
		goto punt;
	}
	tf2 = unix_tfile(fd);

	(void) sprintf(subject,"%s meeting",my_minfo.long_name);
	dsc_add_trn(nbpdest, tf2, subject, 0, txn_no, code_ptr);

punt:
	if (tf2 != NULL)
	     tclose(tf2, &mycode);
	if (fp != NULL)
	     fclose(fp);
	if (my_minfo.chairman != NULL)
	     free(my_minfo.chairman);
	if (my_minfo.location != NULL)
	     free(my_minfo.location);
	if (my_minfo.long_name != NULL)
	     free(my_minfo.long_name);
}
