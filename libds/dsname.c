/*
 *
 * dsname.c -- Routines to implement the discuss name routines, on the
 *	       client end.
 *
 */

#include <strings.h>
#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <sys/file.h>
#include <sys/param.h>
#include <stdio.h>
#include "../include/dsname.h"

#define NULL 0

struct nment {
     char *nm_name;
     int  last;
     int  date_attended;
     char *unique_id;
};

struct nment *getnment();
char *malloc();

extern int errno;

FILE *nm_file;


/*
 *
 * get_mtg_location --  Maps unique_id -> {host, pathname}.  Cheats by
 *			relying on the form of unique_id's.
 *
 */
get_mtg_location (unique_id, host, pathname, result)
char *unique_id;		/* input */
char **host;			/* output, malloc'd */
char **pathname;		/* output, malloc'd */
int *result;			/* standard code */
{
     char *cp;
     int n;

     *result = 0;		/* optimist */

     cp = index (unique_id, ':');
     if (cp == 0) {		/* something's wrong here */
	  *host = NULL;
	  *pathname = NULL;
	  *result = EINVAL;
	  return;
     }

     n = cp - unique_id;
     *host = malloc (n+1);
     bcopy (unique_id, *host, n);
     (*host) [n] = '\0';

     cp++;
     while (isdigit(*cp))
	  cp++;
     n = strlen (cp);
     *pathname = malloc (n+1);
     bcopy (cp, *pathname, n);
     (*pathname) [n] = '\0';

     return;
}

expand_mtg_set (realm, user, mtgname, set, num)
char *realm, *user, *mtgname;	/* input */
name_blk **set;			/* array of name_blk's */
int *num;
{
     int star_realm, star_user, star_mtgname;
     int realm_len, user_len, mtgname_len;
     int count = 0;
     name_blk *nbp;
     struct nment *nm;

     *set = 0;
     *num = 0;
     
     realm_len = strlen (realm);
     user_len = strlen (user);
     mtgname_len = strlen (mtgname);
     if (realm_len   >= NB_REALM_SZ ||
	 user_len    >= NB_USER_SZ ||
	 mtgname_len >= NB_MTG_NAME_SZ)
	  return;				/* he loses */

     star_realm = !strcmp (realm, "*");
     star_user = !strcmp (user, "*");
     star_mtgname = !strcmp (mtgname, "*");

     setnment(user);
     while ((nm = getnment()) != NULL) {
	  if (star_mtgname || !strcmp (mtgname, nm -> nm_name))
	       count++;
     }

     if (count == 0)
	  return;

     *set = (name_blk *) malloc (count * sizeof (name_blk));
     if (*set == 0)
	  return;

     *num = count;
     nbp = *set;
     setnment(user);
     while ((nm = getnment ()) != NULL) {
	  if (star_mtgname || !strcmp (mtgname, nm -> nm_name)) {
	       strcpy (nbp->realm, realm);
	       strcpy (nbp->mtg_name, nm -> nm_name);
	       strcpy (nbp->user, user);
	       strcpy (nbp->unique_id, nm -> unique_id);
	       nbp -> date_attended = nm -> date_attended;
	       nbp -> last = nm -> last;
	       nbp++;
	  }
     }
     return;
}

get_mtg_unique_id (realm, user, mtgname, nb, result)
char *realm, *user, *mtgname;   /* input */
name_blk *nb;			/* output */
int *result;			/* standard code */
{
     int realm_len, user_len, mtgname_len;
     int count = 0;
     struct nment *nm;

     realm_len = strlen (realm);
     user_len = strlen (user);
     mtgname_len = strlen (mtgname);
     if (realm_len   >= NB_REALM_SZ ||
	 user_len    >= NB_USER_SZ ||
	 mtgname_len >= NB_MTG_NAME_SZ) {
	  *result = ENOMEM;
	  return;				/* he loses */
     }

     setnment(user);
     while ((nm = getnment ()) != NULL) {
	  if (!strcmp (mtgname, nm -> nm_name)) {
	       strcpy (nb->mtg_name,mtgname);
	       strcpy (nb->realm, realm);
	       strcpy (nb->user, user);
	       strcpy (nb->unique_id, nm -> unique_id);
	       nb -> date_attended = nm -> date_attended;
	       nb -> last = nm -> last;
	       *result = 0;
	       return;
	  }
     }
     *result = ENOENT;
     return;
}
/*
 * Find name of .disrc file; search path is:
 *	$DISRC environment variable
 *	$HOME/.disrc
 *	<pw->pw_dir>/.disrc
 * This function is "sticky"; it only evaluates the filename once.
 */

static set_rc_filename(user, buf, len)
     char *user, *buf;
     int len;
{
     static char disrcbuf[MAXPATHLEN], *disrcfile = NULL;
     struct passwd *pw;
     register char *cp = NULL;
     extern char *getenv();

     if (*user != '\0') {
          pw = getpwnam(user);
	  if (!pw) {
	       fprintf(stderr, "Unknown user %s\n", user); /* XXX - should return error code */
	       strncpy(buf, "/dev/null", len);
	  } else {
	       strncpy (buf, pw -> pw_dir, len);
	       strncat (buf, "/.disrc", len);
	  }
	  return;
     } 
     if (!disrcfile) {
	  if ((cp = getenv("DISRC")) && !access(cp, R_OK|W_OK)) {
	       strncpy(disrcbuf, cp, MAXPATHLEN-1);
	  } else if ((cp = getenv("HOME")) 
		 &&  (strncpy(disrcbuf,cp, MAXPATHLEN-1))
		 &&  (strncat(disrcbuf,"/.disrc", MAXPATHLEN-1))
		 &&  (!access(disrcbuf, R_OK|W_OK))) {
	       /* got it */
	  } else {
	       pw = getpwuid(getuid());
	       if (!pw) {
		    printf("Who are you?\n"); /* XXX - use warning */
		    strncpy(disrcbuf, "/tmp/.disrc");
	       } else {
		    strncpy (disrcbuf, pw -> pw_dir, MAXPATHLEN-1);
		    strncat (disrcbuf, "/.disrc", MAXPATHLEN-1);
	       }
	  }
	  disrcfile = disrcbuf;
     }
     strncpy(buf, disrcfile, len);	  
}


update_mtg_set(realm, user, set, num, result)
char *realm, *user;		/* input */
name_blk *set;			/* array of name_blk's */
int num;			/* number in set */
int *result;			/* error code */
{
     name_blk *nbp;
     int i;
     char *touched;		/* array of booleans */
     char old_name[MAXPATHLEN], new_name[MAXPATHLEN];
     FILE *new_file;
     struct passwd *pw;
     struct nment *nm;

     *result = 0;

     set_rc_filename(user, old_name, sizeof(old_name));

     strcpy (new_name, old_name);
     strcat (new_name, "~");		/* emacsish, but who cares? */

     new_file = fopen (new_name, "w+");
     if (new_file == NULL) {
	  *result = errno;
	  return;
     }

     touched = malloc (num);
     for (i = 0; i < num; i++)
	  touched[i] = 0;


     setnment(user);
     while ((nm = getnment()) != NULL) {
	  /* walk through user structures, seeing if we find matching entries */
	  for (i = 0, nbp = set;  i < num; i++, nbp++) {
	       if (!strcmp (nm -> unique_id, nbp -> unique_id)) {	/* match, update */
		    nm -> last = nbp -> last;
		    nm -> date_attended = nbp -> date_attended;
		    if (!strcmp (nm -> nm_name, nbp -> mtg_name))
			 touched[i] = 1;
	       }
	  }
	  fprintf(new_file, "%s:%d:%d:%s\n", nm -> nm_name, nm -> last,
		  nm -> date_attended, nm -> unique_id);
     }

     /* clean up ones we haven't touched in memory yet */
     for (i = 0, nbp = set; i < num; i++, nbp++) {
	  if (!touched[i]) {
	       fprintf(new_file, "%s:%d:%d:%s\n", nbp -> mtg_name, nbp -> last,
		  nbp -> date_attended, nbp -> unique_id);
	  }
     }
     endnment();
     fclose(new_file);

     if (rename(new_name, old_name) < 0)
	  *result = errno;
     
     free(touched);
     return;
}



/*
 *
 * getnment () -- Read the name entry
 *
 */
static struct nment *getnment()
{
     static char buffer[512];
     static struct nment nm;
     char *cp;
     int len;

     if (nm_file == NULL) {
	  return (NULL);
     }

     while (fgets (buffer, sizeof(buffer), nm_file) != NULL) {
	  len = strlen(buffer);
	  buffer[len-1] = '\0';
	  nm.nm_name = buffer;
	  cp = index (nm.nm_name, ':');
	  if (cp == 0)
	       continue;
	  *cp++ = '\0';
	  if (!isdigit(*cp))
	       continue;
	  nm.last = 0;
	  while (isdigit(*cp))
	       nm.last = nm.last * 10 + *cp++ - '0';
	  if (*cp++ != ':')
	       continue;
	  if (!isdigit(*cp))
	       continue;
	  nm.date_attended = 0;
	  while (isdigit(*cp))
	       nm.date_attended = nm.date_attended * 10 + *cp++ - '0';
	  if (*cp++ != ':')
	       continue;
	  nm.unique_id = cp;
	  return(&nm);
     }
     return(NULL);
}

/*
 *
 * setnment(user) -- Start reading a file.
 *
 */
static setnment(user)
char *user;
{
     static char my_user[NB_USER_SZ] = "";
     static int got_my_user = 0;
     char buffer[MAXPATHLEN];
     struct passwd *pw;

     if (nm_file == NULL || !got_my_user) {
	  set_rc_filename(user, buffer, sizeof(buffer));
	  nm_file = fopen (buffer, "r");
	  if (nm_file != NULL) {
	       got_my_user = 1;
	       strcpy (my_user, user);
	  }
     }
     if (nm_file != NULL)
	  rewind(nm_file);
     return;
}

static endnment()
{
     if (nm_file != NULL) {
	  fclose (nm_file);
	  nm_file = NULL;
     }
}

