/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/dsname.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/dsname.c,v 1.12 1987-08-08 02:27:59 spook Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 */

#ifndef lint
static char *rcsid_dsname_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/dsname.c,v 1.12 1987-08-08 02:27:59 spook Exp $";
#endif lint

/*
 * db: Implements user's meetings database.
 *
 */

#include <stdio.h>
#include <strings.h>
#include <pwd.h>
#include <sys/file.h>
#include <sys/param.h>
#include <errno.h>
#include "config.h"
#include "dsname.h"
#include "dsc_et.h"

/*
 * Format of data file:
 *    status:last_time:last_seen:uid:name1:name2:name3:....:nameN:
 */

static FILE *db = (FILE *)NULL;
static char *db_file = (char *)NULL;
static char *db_user_id = (char *)NULL;

static server_name_blk current = {
	(char *)NULL, (char *)NULL, (char *)NULL, 0, 0, 0
};

extern char *malloc();
extern int errno;
extern char *local_realm();

static char disrcbuf[MAXPATHLEN]; /* user's MEETINGS file */
static char *disrcfile = NULL;	/* pointer to above */
static char *me = NULL;		/* user's own user_id field */

static char mtgs[] = "/.meetings";

/*
 * Attempt to locate user's .meetings file.  This is intended to be
 * used as a test routine from an application.
 *
 * search path is:
 *	$MEETINGS environment variable
 *	$HOME/.meetings
 *	<pw->pw_dir>/.meetings
 * This function is "sticky"; it only evaluates the filename once.
 */

extern char *getenv();

int find_rc_filename()
{
	struct passwd *pw = NULL;
	register char *cp;

	if (disrcfile)
		return 0;

	pw = getpwuid(getuid());
	if (!pw)
		return NO_SUCH_USER;
	me = malloc(strlen(pw->pw_name)+2+
		    strlen(local_realm()));
	strcpy(me, pw->pw_name);
	strcat(me, "@");
	strcat(me, local_realm());

	cp = getenv("MEETINGS");
	if (cp)
		strcpy(disrcbuf, cp);
	if (!cp) {
		cp = getenv("HOME");
		if (cp) {
			strcpy(disrcbuf, cp);
			strcat(disrcbuf, mtgs);
		}
	}
	if (!cp) {
		strcpy(disrcbuf, pw->pw_dir);
		strcat(disrcbuf, mtgs);
	}
	if (!access(disrcbuf, R_OK|W_OK)) {
		disrcfile = disrcbuf;
		return 0;
	}
	return errno;
}

static int set_rc_filename(auser_id, buf, len)
	char *auser_id, *buf;
	int len;
{
	struct passwd *pw = NULL;
	register char *cp = NULL;

	if ((auser_id == NULL) || 
	    (auser_id[0] == '\0') ||
	    (me && !strcmp(auser_id, me))) {
		if (!disrcfile) {
			register int code;
			if (code = find_rc_filename())
				return code;
		}
        	strncpy(buf, disrcbuf, MAXPATHLEN-1);
		return 0;
	}
	cp = index(auser_id, '@');
	if (cp)
		*cp = '\0';
	pw = getpwnam(auser_id);
	if (cp)
		*cp = '@';
	if (!pw) {
		return NO_SUCH_USER;
	}
	strncpy(buf, pw->pw_dir, len);
	strncat(buf, mtgs, len - strlen(buf));
	return (access(buf, R_OK)? NO_MTGS_FILE : 0);
}

/*
 * ds() -- duplicate a string
 */

static char *
ds(s)
	register char *s;
{
	register int len = strlen(s) + 1;
	register char *ns = malloc(len);
	bcopy(s, ns, len);
	return(ns);
}

static void
enddbent()
{
	if (db) {
		fclose(db);
		db = (FILE *)NULL;
		free(db_file);
		db_file = (char *)NULL;
		free(db_user_id);
		db_user_id = (char *)NULL;
	}
}

/*
 * getdbent() -- get the next entry out of the file.  returns
 * zero on end of file or uncorrectable error, one on success, minus
 * one on correctable error.
 */

static int
getdbent()
{
	char buffer[BUFSIZ];
	char *bufp, *cp;

	if (!db) {
		errno = NO_MTGS_FILE;
		return(0);
	}
	if (!fgets(buffer, BUFSIZ, db)) {
		return 0;
	}

	bufp = index(buffer, '\n');
	if (bufp)
		*bufp = '\0';
	bufp = buffer;

	/* meeting status flags (per-user) */
	current.status = atoi(bufp);
	bufp = index(bufp, ':');
	if (!bufp) {
	bad_fmt:
		errno = BAD_MTGS_FILE;
		return(-1);
	}
	else
		bufp++;

	/* date user last attended meeting */
	current.date_attended = atoi(bufp);
	bufp = index(bufp, ':');
	if (!bufp)
		goto bad_fmt;
	else
		bufp++;

	/* last transaction seen */
	current.last = atoi(bufp);
	bufp = index(bufp, ':');
	if (!bufp)
		goto bad_fmt;
	else
		bufp++;

	/* hostname of meeting */
	if (current.hostname)
		free(current.hostname);
	current.hostname = ds(bufp);
	cp = index(current.hostname, ':');
	if (cp)
		*cp = '\0';
	bufp = index(bufp, ':');
	if (!bufp)
		goto bad_fmt;
	else
		bufp++;

	/* pathname of meeting on remote host */
	if (current.pathname)
		free(current.pathname);
	current.pathname = ds(bufp);
	cp = index(current.pathname, ':');
	if (cp)
		*cp = '\0';
	bufp = index(bufp, ':');
	if (!bufp)
		goto bad_fmt;
	else
		bufp++;

	/* list of aliases for meeting */
	if (current.alias_list)
		free(current.alias_list);
	current.alias_list = ds(bufp);
	cp = index(current.alias_list, ':');
	if (cp)
		*cp = '\0';
	bufp = index(bufp, ':');
	if (!bufp)
		goto bad_fmt;
	else
		bufp++;

	current.spare = ds(bufp);

	return(1);
}

static int
setdbent(user_id)
	char *user_id;
{
	char *auid;
	register int code;

	enddbent();

	auid = malloc(strlen(user_id)+2);
	strcpy(auid, user_id);
	if (!db_file)
		db_file = malloc(MAXPATHLEN);
	if(code = set_rc_filename(user_id, db_file, MAXPATHLEN))
		return code;

	db = fopen(db_file, "r");
	if (!db)
		return(errno);
	if (db_user_id)
		free(db_user_id);
	db_user_id = ds(user_id);
	if (current.user_id)
		free(current.user_id);
	current.user_id = ds(user_id);
	return(0);
}

static int
is_a_name(name)
	register char *name;
{
	register int len;
	register char *ns = current.alias_list;

	if (*name == '*')
		return(1);
	len = strlen(name);

	while (1) {
		if (!strncmp(name, ns, len) && (!ns[len] || ns[len] == ',')) {
			return(1);
		}
		ns = index(ns+1, ',');
		if (!ns || !ns[1]) {
			return(0);
		}
		ns++;
	}
}

static
char **
expand(list)
	char *list;
{
        char *calloc();
	register int num = 1;
	register char *cp;
	register char **rv, **rv1;
	for (cp = list; cp;) {
		num++;
		cp = index(cp, ',');
		if (cp)
			cp++;
	}
	rv = (char **) calloc (num, sizeof(char *));
	rv1 = rv;
	while (list && *list) {
		while (*list == ',')
			list++;
		cp = index(list, ',');
		if (cp)
			*cp = '\0';
		*rv1 = ds(list);
		rv1++;
		if (cp) {
			*cp = ',';
			cp++;
			list = cp;
		}
		else list = (char *) NULL;
	}
	*rv1 = (char *) NULL;
	return(rv);
}

static
char *
compress(list)
	char **list;
{
	int len;
	char **cp;
	char *rv;
	len = 1;
	for (cp = list; *cp; cp++)
		len += 1 + strlen(*cp);
	rv = malloc (len);
	strcpy(rv, *list);
	for (cp = list+1; *cp; cp++) {
		strcat(rv, ",");
		strcat(rv, *cp);
	}
	return (rv);
}

dsc_expand_mtg_set(user_id, name, set, num, result)
	char *user_id;		/* userid to do lookup for */
	char *name;		/* user's name for meeting */
	name_blk **set;		/* returned values */
	int *num;		/* number of returned meetings */
	int *result;		/* return code */
{
	int count;
	register int r;

	*set = 0;
	*num = 0;
	*result = 0;
	r = setdbent(user_id);
	if (r) {
		*result = r;
		return;
	}
	count = 0;
	while ((r=getdbent()) > 0) {
		if (is_a_name(name))
			count++;
	}
	if (r) {		/* getdbent returns -1 */
		*result = errno;
		return;
	}
	r = setdbent(user_id);
	if (r) {
		*result = r;
		return;
	}
	*set = (name_blk *) malloc(count * sizeof(name_blk));
	if (*set == (name_blk *)NULL) {
		*result = errno;
		return;
	}
	while ((r=getdbent()) > 0)
		if (is_a_name(name)) {
			register name_blk *nb = *set + (*num)++;
			nb->date_attended = current.date_attended;
			nb->last = current.last;
			nb->status = current.status;
			nb->hostname = ds(current.hostname);
			nb->pathname = ds(current.pathname);
			nb->aliases = expand(current.alias_list);
			nb->spare = ds(current.spare);
			nb->user_id = ds(user_id);
		}
	if (r)
		*result = r;
}

dsc_get_mtg (user_id, name, nbp, result)
	char *user_id;
	char *name;
	name_blk *nbp;
	int *result;
{
	name_blk *set;
	int num;
	dsc_expand_mtg_set(user_id, name, &set, &num, result);
	if (num == 0) {
		if (!*result)
			*result = NO_SUCH_MTG;
		return;
	}
	bcopy(&set[0], nbp, sizeof(name_blk));
}

static char format[] = "%d:%d:%d:%s:%s:%s:%s\n";

dsc_update_mtg_set(user_id, set, num, result)
	char *user_id;		/* input */
	name_blk *set;		/* array of name_blk's */
	int num;		/* number in set */
	int *result;		/* error code */
{
	name_blk *nbp;
	int i;
	register int r;
	char *touched;		/* array of booleans */
	char *new_name;
	FILE *new_file;
	char *old_name;

	if (*result = setdbent(user_id)) {
		if (*result != NO_MTGS_FILE &&
		    *result != ENOENT)
			return;
		else *result = 0;
	}
	
	new_name = malloc(strlen(db_file) + 2);
	strcpy (new_name, db_file);
	strcat (new_name, "~");		/* emacsish, but who cares? */
	old_name = malloc(strlen(db_file) + 1);
	strcpy (old_name, db_file);

	new_file = fopen (new_name, "w+");
	if (new_file == NULL) {
		*result = errno;
		free(new_name);
		return;
	}
	
	touched = malloc (num);
	for (i = 0; i < num; i++)
		touched[i] = 0;
	
	if (*result = setdbent(user_id)) {
		if (*result != NO_MTGS_FILE &&
		    *result != ENOENT)
			return;
		else *result = 0;
	}

	while ((r=getdbent()) != 0) {
		if (r == -1)	/* if bad format, punt it */
			continue;
		/* walk through user structures, look for matching entries */
		for (i = 0, nbp = set;  i < num; i++, nbp++) {
			if (!strcmp (current.hostname, nbp -> hostname) && !strcmp(current.pathname, nbp->pathname)) {
				/* match, update */
				current.last = nbp -> last;
				current.date_attended =
					nbp -> date_attended;
				current.status = nbp -> status;
				current.alias_list = compress(nbp -> aliases);
				current.spare = nbp->spare;
				touched[i] = 1;
			}
		}
		if (current.status & DSC_ST_DELETED) continue;

		if (fprintf(new_file, format,
			current.status, current.date_attended, current.last,
			current.hostname, current.pathname,
			current.alias_list, current.spare) == EOF)
		     goto punt;
	}
	
	/* clean up ones we haven't touched in memory yet */
	for (i = 0, nbp = set; i < num; i++, nbp++) {
		if (!touched[i]) {
			if(fprintf(new_file, format,
				nbp->status, nbp->date_attended, nbp->last,
				nbp->hostname, nbp->pathname,
				compress(nbp->aliases), current.spare) == EOF)
			     goto punt;
		}
	}
	enddbent();
	if (fclose(new_file)==EOF)
	     goto punt2;
	*result = (rename(new_name, old_name) < 0) ? errno : 0;
	free(new_name);
	free(old_name);
	free(touched);
	return;

punt:
	fclose(new_file);
punt2:
	enddbent();
	unlink(new_name);
	free(new_name);
	free(old_name);
	free(touched);
	*result = CANT_WRITE_TEMP;
	return;
	
}
