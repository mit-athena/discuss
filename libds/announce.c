/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v 1.2 1987-04-09 00:12:23 rfrench Exp $
 *	$Locker:  $
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.1  87/04/08  21:41:30  rfrench
 * Initial revision
 * 
 */

#ifndef lint
static char *rcsid_announce_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v 1.2 1987-04-09 00:12:23 rfrench Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include "tfile.h"
#include "dsname.h"
#include "dsc_et.h"

#define min(x,y) ((x)<(y)?(x):(y))

dsc_announce_mtg (nbpsrc, nbpdest, public, tf, txn_no, code_ptr)
name_blk *nbpsrc, *nbpdest;
int public;
tfile tf;
int *txn_no;
int *code_ptr;
{
	char temp_file[64];
	char buffer[512],subject[100];
	int fd,tfs,tocopy;
	FILE *fp;
	tfile tf2;

	*code_ptr = 0;

	(void) sprintf(temp_file,"/tmp/mtgz%d.%d",getuid(),getpid());
	(void) unlink(temp_file);

	fp = fopen(temp_file,"w");
	if (!fp) {
		*code_ptr = CANT_WRITE_TEMP;
		return;
	}
	fprintf(fp,"  Meeting Name:  %s\n", nbpsrc->aliases[0]);
	fprintf(fp,"  Host:          %s\n", nbpsrc->hostname);
	fprintf(fp,"  Pathname:      %s\n", nbpsrc->pathname);
	fprintf(fp,"  Participation: %s\n", public?"Public":"Private");
	fprintf(fp,"\n");
	fclose(fp);

	fd = open(temp_file,O_APPEND|O_RDWR,0);
	if (fd < 0) {
		*code_ptr = CANT_WRITE_TEMP;
		return;
	}
	tf2 = unix_tfile(fd);
	tfs = tfsize(tf);
	while (tfs > 0) {
		tocopy = min (512, tfs);
		tocopy = tread (tf, buffer, tocopy, code_ptr);
		if (*code_ptr)
			return;
		twrite (tf2, buffer, tocopy, code_ptr);
		if (*code_ptr)
			return;
		tfs -= tocopy;
	}
	(void) fclose(fp);
	(void) tclose(tf2);

	fd = open(temp_file,O_RDONLY,0);
	if (fd < 0) {
		*code_ptr = CANT_WRITE_TEMP;
		return;
	}
	tf2 = unix_tfile(fd);

	(void) sprintf(subject,"%s meeting",nbpsrc->aliases[0]);
	dsc_add_trn(nbpdest, tf2, subject, 0, txn_no, code_ptr);
}
