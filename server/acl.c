/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/acl.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/acl.c,v 1.6 1987-03-17 02:25:19 srz Exp $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 * 	Routines for the manipulation of access control lists in core,
 *	along with routines to move them to and from files.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.5  86/12/08  23:30:56  wesommer
 * Changed acl_canon() to line up columns.
 * 
 * Revision 1.4  86/12/04  10:18:57  srz
 * You !#@$% C programmers don't know what negative subscripts are! ;-)
 * 
 * Revision 1.3  86/11/22  06:17:45  spook
 * Changed to make lint happy; also punted duplicate boolean types.
 * 
 * Revision 1.2  86/11/16  06:01:47  wesommer
 * Implemented acl_replace_access.
 * Redefined acl_delete_access and diked out the old one.
 * Implemented acl_canon (canonicalizes ACL mode string).
 * Implemented modularity-violating fdclose (blech) so we can use stdio 
 * with fdopen in acl_{read,write} and not lose FILE *'s to a storage leak.
 * 
 */

#ifndef lint
static char *rcsid_acl_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/acl.c,v 1.6 1987-03-17 02:25:19 srz Exp $";
#endif lint

#include "../include/acl.h"
#include "../include/dsc_et.h"
#include <stdio.h>
#include <strings.h>

char *malloc(), *realloc();
char *acl_union(), *acl_intersection(), *acl_subtract();

bool acl_check(list, principal, modes)
     Acl *list;
     char *principal;
     char *modes;
{
	register acl_entry *ae;
	register int n;
	for (ae = list->acl_entries, n=list->acl_length;
	     n;
	     ae++, n--) {
		if ((strcmp(principal, ae->principal) == 0) ||
		    (strcmp("*", ae->principal) == 0))
			return(acl_is_subset(modes, ae->modes));
	}
	return (FALSE);
}

Acl *acl_read(fd)
     int fd;
{
	static char buf[128];
	FILE *f=fdopen(fd, "r");
	register char *cp;
	register int n;
	register acl_entry *ae;
	register Acl *list = (Acl *) malloc(sizeof(Acl));
	fgets(buf, 128, f);
	n=list->acl_length=atoi(buf);
	list->acl_entries = (acl_entry *)malloc((unsigned)(n * sizeof(acl_entry)));

	for (ae=list->acl_entries; n; --n, ++ae)  {
		buf[0]=0;
		fgets(buf, 128, f);
		if(cp=index(buf, '\n')) *cp='\0';
		if(cp=index(buf, ':')) {
			*cp='\0';
			ae->modes = malloc((unsigned)(strlen(buf)+1));
			(void) strcpy(ae->modes, buf);
			ae->principal = malloc((unsigned)(strlen(cp+1)+1));
			(void) strcpy(ae->principal, cp+1);
		} else { /* skip line */
			list->acl_length--;
			--ae;
		}
	}
	(void) fdclose(f); /*XXX*/
	return(list);
}

bool acl_write(fd, list)
     int fd;
     Acl *list;
{
	FILE *f=fdopen(fd, "w");
	register int n;
	register acl_entry *ae;
	(void) fprintf(f, "%d\n", list->acl_length);
	for (ae=list->acl_entries, n=list->acl_length; n; --n, ++ae)
		(void) fprintf(f, "%s:%s\n", ae->modes, ae->principal);
	(void) fdclose(f); /*XXX*/
	return(TRUE);
}

acl_add_access(list, principal, modes)
     Acl *list;
     char *principal;
     char *modes;
{
	register int n;
	register acl_entry *ae;
	char *new_modes;
	for(ae=list->acl_entries, n=list->acl_length; n; --n, ++ae) {
		if (!strcmp(ae->principal, principal)) {
			new_modes = acl_union(modes, ae->modes);
			(void) free(ae->modes);
			ae->modes=new_modes;
			return;
		}
	}
	/*
	 * Whoops.. fell through.
	 * Realloc the world - or at least the vector.
	 */
	list->acl_length++;
	list->acl_entries = (acl_entry *) realloc((char *)(list->acl_entries),
						  (unsigned)(list->acl_length * sizeof(acl_entry)));
	ae = list->acl_entries + list->acl_length - 2;
	/*
	 * Is the last entry "*"?  If so, push it back one.
	 */
	if (list -> acl_length > 1) {		/* non-empty acl */
	     if (!strcmp(ae->principal, "*")) {
		  if(!strcmp(principal, "*")) 
			  panic("acl broke");
		  *(ae+1) = *ae;
		  --ae;
	     }
	}
	ae++;
	ae->principal = malloc((unsigned)(strlen(principal)+1));
	(void) strcpy(ae->principal, principal);
	ae->modes = malloc((unsigned)(strlen(modes)+1));
	(void) strcpy(ae->modes, modes);
}

#ifdef notdef
acl_delete_modes(list, principal, modes)
     Acl *list;
     char *principal;
     char *modes;
{
	register acl_entry *ae, *ae1;
	register int n;
	char *new_modes;
	for(ae=list->acl_entries, n=list->acl_length; n; --n, ++ae) {
		if (!strcmp(ae->principal, principal)) {
			new_modes = acl_subtract(modes, ae->modes);
			(void) free(ae->modes);
			ae->modes=new_modes;
			goto cleanup;
		}
	}
	return;
cleanup:
	ae1 = list->acl_entries + list->acl_length - 1;
	if ((strcmp(ae1->principal, "*")!= 0 && strcmp(ae->modes, "")) ||
	    (!strcmp(ae1->principal, "*") && !strcmp(ae->modes, ae1->modes))) {
		    (void) free(ae->principal);
		    (void) free(ae->modes);
		for (; ae <ae1; ae++) *ae= *(ae+1);
		list->acl_length--;
		list->acl_entries = (acl_entry *) realloc(list->acl_entries,
				       list->acl_length * sizeof(acl_entry));
	}

}	       
#endif notdef
acl_replace_access(list, principal, modes)
     Acl *list;
     char *principal;
     char *modes;
{
	register acl_entry *ae;
	register int n;
	for(ae=list->acl_entries, n=list->acl_length; n; --n, ++ae) {
		if (!strcmp(ae->principal, principal)) {
			(void) free(ae->modes);
			ae->modes = malloc((unsigned)(strlen(modes)+1));
			(void) strcpy(ae->modes, modes);
			return;
		}
	}
	/* Didn't find it.  Insert it.. the easy way. */
	acl_add_access(list, principal, modes);
}

bool
acl_delete_access(list, principal)
	Acl *list;
	char *principal;
{
	register acl_entry *ae;
	register int n;
	for(ae=list->acl_entries, n=list->acl_length; n; --n, ++ae) {
		if (!strcmp(ae->principal, principal)) {
			/* gotcha! */
			list->acl_length--;
			while (--n) {
				*ae= *(ae+1);
				ae++;
			}
			return TRUE;
		}
	}
	return FALSE;
}
/*
 * Return empty ACL
 */

Acl *acl_create()
{
	register Acl *result = (Acl *) malloc(sizeof(Acl));
	result->acl_length=0;
	result->acl_entries=(acl_entry *) malloc(sizeof(acl_entry));
	return(result);
}

Acl *acl_copy (old_acl)
Acl *old_acl;
{
     register Acl *new_acl = (Acl *) malloc (sizeof(Acl));
     register acl_entry *old_ae,*new_ae;
     register int n;

     new_acl -> acl_length = old_acl -> acl_length;
     new_acl -> acl_entries = (acl_entry *) malloc ((unsigned)(new_acl -> acl_length * sizeof(acl_entry)));

     for (old_ae = old_acl->acl_entries, new_ae = new_acl->acl_entries, n=new_acl->acl_length;
	  n;
	  --n, ++old_ae, ++new_ae) {
	       new_ae->principal = malloc (strlen(old_ae->principal)+1);
	       strcpy (new_ae->principal,old_ae->principal);
	       new_ae->modes = malloc (strlen(old_ae->modes)+1);
	       strcpy (new_ae->modes,old_ae->modes);
	  }
	       
     return (new_acl);
}



char *acl_get_access(list, principal)
	Acl *list;
	char *principal;
{
	register acl_entry *ae;
	register int n;

	for(ae=list->acl_entries, n=list->acl_length; n; --n, ++ae) {
		if (!strcmp(ae->principal, principal) ||
		    !strcmp(ae->principal, "*")) {
			return(ae->modes);
		}
	}
	return("");
}
/*
 * Destroy an ACL.     
 */

acl_destroy(list)
     Acl *list;
{
	register acl_entry *ae;
	register int n;
	if(!list) return;
	for (ae=list->acl_entries, n=list->acl_length;
	     n;
	     --n, ++ae) {
		     (void) free(ae->principal);
		     (void) free(ae->modes);
	}
	(void) free((char *)(list->acl_entries));
	(void) free((char *)list);
}

/*
 * Returns true if every character of s1 occurs in s2.
 */
bool acl_is_subset(s1, s2)
     register char *s1, *s2;
{
	register char *last;
	while(*s1 && (last = index(s2, *s1))) s1++;

	return(last != NULL);
}

/*
 * Returns the intersection of the two strings s1 and s2.
 * This function allocates a new string, which should be freed 
 * when not needed.
 */
char *acl_intersection(s1, s2)
     register char *s1, *s2;
{
	register char *result=malloc(1);
	register int resp=0;

	while(*s1) {
		if(index(s2, *s1)) {
			result[resp++] = *s1;
			result=realloc(result, (unsigned)(resp+1));
		}
		s1++;
	}
	result[resp] = '\0';
	return(result);
}

char *acl_union(s1, s2)
     register char *s1, *s2;
{
	register int resp=strlen(s2);
	register char *result=malloc((unsigned)(resp+1));
	strcpy(result, s2);
	while(*s1) {
		if (!index(result, *s1)) {
			result[resp++]= *s1;
			result=realloc(result, (unsigned)(resp+1));
		}
		s1++;
	}
	result[resp]='\0';
	return(result);
}

char *acl_subtract(s1, s2)
     register char *s1, *s2;
{
	register char *result = malloc(1);
	register int len=0;
	for (; *s2; s2++) {
		if (!index(s1, *s2)) {
			result = realloc(result, (unsigned)(len+2));
			result[len++]= *s2;
		}
	}
	result[len]='\0';
	return(result);
}

/*
 * Canonicalize an acl string; sort the characters of s1 into 
 * the order found in s2, removing duplicates, and returning an error code if 
 * any of them are not in s2.
 * This is a sucky algorithm, but who really gives?
 */

char *acl_canon(s1, s2, code)
	register char *s1, *s2;
	int *code;
{
	register char *cp;
	register char *out;
	register int len, maxlen;

	*code = 0;
	for (cp = s1; *cp; cp++) {
		if (*cp != ' ' && !index(s2, *cp)) 
			*code = BAD_MODES;
	}
	maxlen = strlen(s2);
	out = malloc(maxlen + 1); len = 0;
	for (cp = s2; *cp; cp++) {
		len++;
		if (len > maxlen) /* shouldn't happen, but.. */
			out = realloc(out, (unsigned)len + 1);

		out[len-1] = (index(s1, *cp) ? *cp : ' ');
	}
	out[len]='\0';
	return(out);
}

	


#ifdef TESTS
#include <sys/file.h>
main() 
{
	int fd;
	Acl *a;

	printf("%s * %s = %s\n", "eabce", "abcd", 
	       acl_intersection("eabce", "abcd"));
	printf("%s + %s = %s\n", "abc", "cbade", acl_union("abc", "cbade"));
	printf("%s < %s = %d\n", "ab", "bcde", acl_is_subset("ab", "bcde"));
	printf("%s < %s = %d\n", "ab", "abcde", acl_is_subset("ab", "abcde"));

	fd = open ("foo.acl", O_RDONLY);
	a=acl_read(fd);
	(void) close(fd);
	printf("a for wesommer: %d\n", acl_check(a, "wesommer", "a"));
	printf("a for foobar: %d\n", acl_check(a, "foobar", "a"));
	printf("a for spook: %d\n", acl_check(a, "spook", "a"));
	printf("g for foobar: %d\n", acl_check(a, "foobar", "g"));
	printf("g for wesommer: %d\n", acl_check(a, "wesommer", "g"));
	printf("g for spook: %d\n", acl_check(a, "spook", "g"));
	printf("d for spook: %d\n", acl_check(a, "spook", "d"));
	printf("d for wesommer: %d\n", acl_check(a, "wesommer", "d"));
	acl_add_access(a, "zot", "qna");
	acl_add_access(a, "spook", "d");
	acl_add_access(a, "*", "w");
	fd = open("bar.acl", O_WRONLY+O_CREAT+O_TRUNC);
	acl_write(fd, a);
	(void) close(fd);
}
#endif TESTS

static
panic(s)
	char *s;
{
	printf(s);
	fflush(stdout);
	abort();
}

/*
 *	Can you say "modularity violation??":  This does an fclose 
 *	without close(2)'ing the file descriptor behind it.
 */

#include <stdio.h>

fdclose(iop)
	register FILE *iop;
{
	register int r;

	r = EOF;
	if (iop->_flag&(_IOREAD|_IOWRT|_IORW) && (iop->_flag&_IOSTRG)==0) {
		r = fflush(iop);
		if (iop->_flag&_IOMYBUF)
			free(iop->_base);
	}
	iop->_cnt = 0;
	iop->_base = (char *)NULL;
	iop->_ptr = (char *)NULL;
	iop->_bufsiz = iop->_flag = iop->_file = 0;
	return(r);
}
