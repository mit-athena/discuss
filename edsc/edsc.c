/*
 *
 * edsc () -- A subprocess to make implement emacs discuss mode easier.
 *
 *	Copyright (C) 1988 by the Student Information Processing Board
 * 
 */


#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/edsc.c,v 1.2 1988-10-26 15:04:59 srz Exp $";
#endif lint

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <pwd.h>
#include <ctype.h>

#include "discuss.h"
#include "config.h"

char *malloc();
char *local_realm();
int log_warn();
tfile unix_tfile();

char *temp_file;
char *pgm;
char *user_id;
tfile stdout_tf;
main(argc, argv)
     int argc;
     char **argv;
{
     int code;
     char input_buf[512];
     char *initial_meeting = (char *)NULL;
     char *cp,*op,delim,*args;

     init_disc_err_tbl();
     init_dsc_err_tbl();

     temp_file = malloc(64);
     pgm = malloc(64);
     (void) sprintf(temp_file, "/tmp/mtg%d.%d", (int)getuid(), getpid());

     if (code = find_rc_filename()) {
	  char buf[100];
	  sprintf(buf, "%s -q", DSC_SETUP);
	  system(buf);
	  if (code = find_rc_filename()) {
	       printf(";%s\n", error_message(code));
	       exit(1);
	  }
     }

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

	  if (gets(input_buf) == NULL)
	       return;

	  if (input_buf[0] != '(') {
bad_syntax:
	       printf("; Incorrect syntax\n");
	       continue;
	  }

	  cp = &input_buf[1];
	  if (get_word(&cp,&op,") ",&delim) < 0)
	       goto bad_syntax;

	  args = cp;
	  /* Depending on the operation, call the routine */
	  if (!strcmp(op, "gmi"))
	       do_gmi(args);
	  else if (!strcmp(op, "quit"))
	       exit(0);
	  else if (!strcmp(op, "gti"))
	       do_gti(args);
	  else if (!strcmp(op, "gcm"))
	       do_gcm(args);
 	  else if (!strcmp(op, "gml"))
	       do_gml(args);
	  else if (!strcmp(op, "gt"))
	       do_gt(args);
	  else if (!strcmp(op, "grt"))
	       do_grt(args);
	  else if (!strcmp(op, "ss"))
	       do_ss(args);
	  else if (!strcmp(op, "at"))
	       do_at(args);
	  else
	       printf("; Unimplemented operation\n");
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
