/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v 1.1 1987-04-08 21:41:30 rfrench Exp $
 *	$Locker:  $
 *
 *	$Log: not supported by cvs2svn $
 */

#ifndef lint
static char *rcsid_announce_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/announce.c,v 1.1 1987-04-08 21:41:30 rfrench Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include "tfile.h"
#include "dsname.h"
#include "dsc_et.h"

#define min(x,y) ((x)<(y)?(x):(y))

dsc_announce_mtg (nbp, long_name, public, tf, txn_no, code_ptr)
name_blk *nbp;
char *long_name;
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
	fprintf(fp,"  Meeting Name:  %s\n", long_name);
	fprintf(fp,"  Host:          %s\n", nbp->hostname);
	fprintf(fp,"  Pathname:      %s\n", nbp->pathname);
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

	(void) sprintf(subject,"%s meeting",long_name);
	dsc_add_trn(nbp, tf2, subject, 0, txn_no, code_ptr);
}
