/*
 *
 * dsmail.c - Modifies/parses mail to discuss
 *
 *        $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/ndsmail.c,v $
 *
 *        $Log: not supported by cvs2svn $
 * Revision 1.7  88/01/15  22:42:39  srz
 * set_module now returns fatal flag, again.
 * 
 * Revision 1.6  88/01/05  03:09:30  srz
 * Put location of DSPIPE into config.h
 * 
 * Revision 1.5  87/10/24  04:26:44  wesommer
 * Rewritten for speed and efficiency.  "popen" is a crock.
 * 
 * Revision 1.4  87/07/17  03:34:57  tytso
 * Bugfixes made, added -p option, removed zippy stuff
 * 
 * Revision 1.3  87/05/02  02:12:58  tytso
 * Replaced the regexp and options parsing code with the UNIX
 * library routines.  Cleaned up code.  Added -A (all headers)
 * option.
 * 
 */

#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <sys/file.h>

#include "tfile.h"
#include "config.h"

#define DEFAULT_SUBJECT "No subject found in mail header"
#define BUFFLEN 400
#define LISTLEN 40

#ifdef __HIGHC__
pragma alloca(on);
#endif

#ifdef __GNUC__
#define alloca(x) __builtin_alloca(x)
#endif

#ifndef	lint
static char rcsid[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/ndsmail.c,v 1.1 1993-10-12 05:58:47 probe Exp $";
#endif

char *malloc();
char *mktemp();
FILE *popen();
FILE *fopen();
FILE *fdopen();
int getopt();

extern char *optarg;                /* External variables for getopt */
extern int optind;

char *deflist[] = {
	"^to$",
	"^from$",
	"^cc$",
	".*-to$",
	".*-from$",
	"^date$",
	"^newsgroups$",
	NULL
};

char *subjlist[] = {
	"subject",NULL
};

char *inreplyto[] = {
	"in-reply-to",NULL
};


char *progname;
char *save[LISTLEN],*reject[LISTLEN];
char *usr_mtg = "";
int 	dodefs=0,
	allfields=0,
	debug;
char *optarg;
int optind;
char *dspipeloc;
extern tfile unix_tfile();
int reply_to;

void PRS();
void MakeLowerCase();
void PipeToMeeting();

int main (argc,argv)
    int argc;
    char *argv[];
{
    FILE *f;
    char line[BUFFLEN+1],*keyword,*key;
    char *subject=NULL;
    int i,ok_prev=0;
    int is_cont = 0;		/* continuation -- too long, or NL-whitespc */
    char *filename;

    PRS (argc,argv);
    line[BUFFLEN] = '\0';
    if (debug) 
	f=stdout;
    else {
	filename = alloca (20);
	strcpy (filename, "/tmp/DSXXXXXX");
	filename = mktemp(filename);
	if ((f = fopen(filename,"w+")) == NULL)
	    perror(filename);
	if (fchmod(fileno(f),0600))
	    perror("chmod temp file");
    }
    if (debug)
	printf("start of text->");
    reply_to = 0;

    if (fgets(line,BUFFLEN,stdin)==NULL)
	exit(0);
    /* Process message header. */
    is_cont = 0;
    while (1) {
	if (is_cont || (isascii (line[0]) && isspace (line[0]))) {
	    if (use_current_header)
		fputs (line, f);
	    if (is_subject) {
		subject = realloc (subject,
				   strlen (subject) + strlen (line) + 3);
		if (!subject) {
		    /* return error on no-memory; temporary */
		    abort ();
		}
		strcat (subject, line);
	}
	else if (!isascii (line[0])) {
	    /* gack! punt, flush this through */
	    fputs (line, f);
	}
	else if (line[0] == '\n')
	    goto end_of_header;
	else if (isspace (line[0])) {
	    /* continuation of previous header line */
	    if (use_current_header)
		fputs (line, f);
	}
	else {
	    /* new header line; parse away */
	    char *cp = line, *cp2;
	    asm ("frep");
	    while (isascii (*cp)
		   && !isspace (*cp)
		   && !iscntrl (*cp)
		   && *cp != ':')
		cp++;
	    if (*cp != ':') {
		fputs (line, f);
		goto message_body;
	    }
	    header = alloca (cp - line + 1);
	    for (cp2 = line; cp2 < cp; cp2++)
		header[cp2-line] = *cp2;
	    header[cp2-line] = '\0';
#if old_code
    while (*line != '\n') {
	if (!isascii(line[0]) || isspace(line[0]))
	    iscont = 1;
	if (!iscont) {
	    for (i = 0; line[i] && line[i] != ':'; i++)
		/* void */;
	    keyword = malloc (i+1);
	    (void) strncpy (keyword,line,i);
	    keyword[i] = '\0';
	    key = malloc (i+1);
	    MakeLowerCase(strcpy(key,keyword));
	    if (list_compare(key,subjlist)) {
		for (i++;line[i]==' ';i++) ;
		/* if luser tries two subject lines, we */
		/* ignore the second subject line */
		if (subject==NULL) {
		    subject=malloc(strlen(line)-i+1);
		    (void) strcpy(subject,line+i);
		}	
	    } else if (list_compare(key, inreplyto)) {
		char *cp;
		if ((cp = index(line,'[')) && index(cp, ']')) {
		    cp++;
		    if (isdigit(*cp))
			reply_to = atoi(cp);
		}
	    }
	}
	if ((ok_prev && iscont) || (!iscont && CheckField(key))) {
	    ok_prev=1;
	    fputs (line, f);
	} else
	    ok_prev=0;
	if (!iscont) {
	    free(key);
	    free(keyword);
	}
	iscont = line[strlen(line)-1] != '\n';
	if (fgets(line,BUFFLEN,stdin)==NULL)
	    goto bye;
    }
    /* keep the blank line */
    fputc ('\n', f);
    /* now send through the rest of the message */
    while (fgets(line,BUFFLEN,stdin)!=NULL) {
	fputs (line, f);
    }
    if (fflush(f) == EOF || ferror(f)) {
	fputs("dsmail: Unable to write to temporary file\n", stderr);
	exit(1);
    }

    PipeToMeeting(f,(subject==NULL) ? DEFAULT_SUBJECT : subject,
		  reply_to);
#endif
}}}
bye:
    if (!debug)
	(void) unlink(filename); 
    exit(0);
}

/* Pipes? I don't see any pipes here.. */
void PipeToMeeting(f,subject, reply_to)
    char *subject;
    FILE *f;
    int reply_to;
{
    int len;
    char module[100];
    int fatal_err,result;
    tfile transaction;
    int trn_no;
    
    strcpy(module, "discuss@");
    gethostname(&module[8], sizeof(module)-8);
    init_rpc();
    set_module(module, &fatal_err, &result);
    if (result) {
	fprintf(stderr, "Can't connect to discuss server: %s\n",
		error_message(result));
	exit(1);
    }
    
    rewind(f); lseek(fileno(f), 0, L_SET);
    transaction = unix_tfile(fileno(f));
    
    len=strlen(subject);
    if (len && (subject[--len] == '\n'))
	subject[len]='\0';
    if (debug) {
	printf("subject is ``%s''\nreplying to %d in %s\n",
	       subject, reply_to, usr_mtg);
    } 
    add_trn(usr_mtg, transaction, subject, reply_to, &trn_no, &result);
    if (result) {
	fprintf(stderr, "Can't enter transaction into %s: %s\n",
		usr_mtg, error_message(result));
	exit(1);
    }
    (void) fclose(f);
}

void MakeLowerCase(s)
    char *s;
{
    int i;
    for (i=0;s[i];i++)
	s[i]=isupper(s[i]) ? tolower(s[i]) : s[i];
}

int CheckField(key)
    char *key;
{
    int keepfield;
    
    keepfield=allfields;
    if (!keepfield && dodefs && list_compare(key,deflist))
	keepfield=1;
    if (!keepfield && list_compare(key,save))
	keepfield=1;
    if (keepfield && list_compare(key,reject))
	keepfield=0;
    return(keepfield);
}

/* Parse command line arguments */
void PRS(argc,argv)
    int argc;
    char **argv;
{
    int c,rp,sp;
    
    progname=argv[0];
    dspipeloc = DSPIPE;
    sp=rp=0;
    optind=1;		/* Initialize for getopt */
    while ((c = getopt(argc,argv,"AZDda:r:p:")) != EOF)
    switch(c) {
    case 'd':
	dodefs=!dodefs;
	break;
    case 'D':
	debug=!debug;
	break;
    case 'A':
	allfields=!allfields;
	break;
    case 'a':
	MakeLowerCase(optarg);
	save[sp++]=optarg;
	if (sp>=LISTLEN) {
	    fprintf(stderr,"Too many accept fields\n");
	    exit(1);
	}
	break;
    case 'r':
	MakeLowerCase(optarg);
	reject[rp++]=optarg;
	if (sp>=LISTLEN) {
	    fprintf(stderr,"Too many reject fields\n");
	    exit(1);
	}
    case 'p':
	dspipeloc = optarg;
	break;
    }    
    if (optind>=argc) 
	goto lusage;
    usr_mtg=argv[optind];
    save[sp]=NULL;		/* Insert terminators */
    reject[rp]=NULL;
    return;
lusage:
    fprintf(stderr,
	    "Usage: %s [-dADZ] [-a field] [-r field] meeting-path-name\n",
	    progname);
    exit(1);
}

char *re_comp();

int list_compare(s,list)
    char *s,**list;
{
    char *err;
    
    while (*list!=NULL) {
	err=re_comp(*list++);
	if (err) {
	    fprintf(stderr,"%s: %s - %s\n",progname,err,*(--list));
	    exit(1);
	}
	if (re_exec(s))
	    return(1);
    }
    return(0);
}

