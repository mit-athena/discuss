/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * edsc () -- A subprocess to make implement emacs discuss mode easier.
 *
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/edsc.c,v 1.10 1996-09-19 22:29:32 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <pwd.h>
#include <ctype.h>
#include <errno.h>
#include "config.h"
#include "edsc.h"
#define INPUT_BUFF_SIZE 10240

char *malloc();
char *local_realm();
int log_warn();
tfile unix_tfile();

static struct edsc_req {
     char *name;				/* Name of request */
     int (*routine)();				/* Routine to call */
} edscr[] = {
	{"quit", do_quit},
	{"gmi", do_gmi},
	{"gti", do_gti},
	{"gml", do_gml},
	{"gt", do_gt},
	{"gtf", do_gtf},
	{"grt", do_grt},
	{"grtn", do_grtn},
	{"ss", do_ss},
	{"at", do_at},
	{"nut", do_nut},
	{"sfl", do_sfl},
	{"am", do_am},
	{"dm", do_dm},
	{"pacl", do_pacl},
	{"dacl", do_dacl},
	{"sacl", do_sacl},
	{"ls", do_ls},
	{"dt", do_dt},
	{"rt", do_rt},
#ifdef EDSC_CACHE
	{"scd", do_scd},
	{"gtfc", do_gtfc},
	{"it", do_it},
	{"itn", do_itn},
	{"im", do_im},
#endif
	{"gpv", do_gpv}
};

#define NUM_EDSC_REQUESTS (sizeof (edscr) / sizeof (struct edsc_req))

/*
 * This is we can cleanup when we have problems
 */
int crash_handler(sig)
	int	sig;
{
	static int	shutting_down_cache = 0;
	int	pid;

	pid = fork();
	/*
	 * If the fork() fails or if this is the child, do a cache shutdown
	 */
	if (pid <= 0) {
		printf("; Edsc crash (code dump in /usr/tmp) --- signal %d\n",
		       sig);
#ifdef EDSC_CACHE
		if (!shutting_down_cache) {
			shutting_down_cache++;
			cache_shutdown();
		}
#endif
	}
	/*
	 * If the fork fails or if this is the parent, cd to /usr/tmp
	 * and perform a crash dump
	 */
	if (pid != 0) {
		(void) chdir("/usr/tmp");
		signal(SIGILL, SIG_DFL);
		abort();
	}
	exit(1);
	/* NOTREACHED */
	return(0);
}

	

char *temp_file;
char *pgm;
char *user_id;
tfile stdout_tf;
main(argc, argv)
     int argc;
     char **argv;
{
     int code,i;
     static char input_buf[INPUT_BUFF_SIZE];
     char *cp,*op,delim,*args;
     struct rlimit limit;

     init_dsc_err_tbl();

     temp_file = malloc(64);
     pgm = malloc(64);
     (void) sprintf(temp_file, "/tmp/mtg%d.%d", (int)getuid(), getpid());

     code = find_rc_filename();
     if (code && (code != EACCES)) {
	  char buf[100];
	  sprintf(buf, "%s -q", DSC_SETUP);
	  system(buf);
	  code = find_rc_filename();
     }
     if (code) {
	     printf(";%s\n", error_message(code));
	     exit(1);
     }
#ifdef EDSC_CACHE
     cache_init(0);
#endif
     /*
      * Set up debugging hooks.  Also useful becuase it means we clean
      * up our cache files in case we die ungracefully.
      */
     getrlimit(RLIMIT_CORE, &limit);
     limit.rlim_cur = limit.rlim_max;
     setrlimit(RLIMIT_CORE, &limit);
#ifdef SIGILL
     signal(SIGILL, crash_handler);
#endif
#ifdef SIGIOT
     signal(SIGIOT, crash_handler);
#endif
#ifdef SIGEMT
     signal(SIGEMT, crash_handler);
#endif
#ifdef SIGFPE
     signal(SIGFPE, crash_handler);
#endif
#ifdef SIGBUS
     signal(SIGBUS, crash_handler);
#endif
#ifdef SIGSEGV
     signal(SIGSEGV, crash_handler);
#endif
#ifdef SIGPIPE
     signal(SIGPIPE, SIG_IGN);
#endif
     /*
      * Set up hooks in case we get a graceful die signal
      */
     signal(SIGHUP, do_quit);
     signal(SIGINT, do_quit);
     signal(SIGTERM, do_quit);
     
     {
	  register char *user = getpwuid(getuid())->pw_name;
	  register char *realm = local_realm();

	  user_id = malloc((unsigned)(strlen(user)+strlen(realm)+2));
	  strcpy(user_id, user);
	  strcat(user_id, "@");
	  strcat(user_id, realm);
     }

     stdout_tf = unix_tfile(1);		/* stdout tfile */

     while (1) {
	  set_warning_hook(log_warn);
#ifdef EDSC_CACHE
	  if (cache_working)
		  do_cache_work();
#endif
	     
	  if (fgets(input_buf, INPUT_BUFF_SIZE, stdin) == NULL)
		  do_quit(0);

	  if (input_buf[0] != '(') {
bad_syntax:
	       printf(";Incorrect syntax\n");
	       continue;
	  }

	  cp = &input_buf[1];
	  if (get_word(&cp,&op,") ",&delim) < 0)
	       goto bad_syntax;

	  args = cp;
	  /* Depending on the operation, call the routine */
	  for (i = 0; i < NUM_EDSC_REQUESTS; i++) {
		  if (!strcmp (op, edscr[i].name)) {
			  (*(edscr[i].routine))(args);
			  break;
		  }
	  }
	  if (i >= NUM_EDSC_REQUESTS)
		  printf(";Unimplemented operation\n");

	  fflush(stdout);
	  }		  
}

log_warn(code, message)
int code;
char *message;
{
     printf("-%s %s\n", error_message(code), message);
}

bit_bucket(code, message)
int code;
char *message;
{
}

do_quit(args)
	char	*args;
{
#ifdef EDSC_CACHE
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	cache_shutdown();
#endif
	exit(0);
}
