/*
 *
 * expunge -- program to expunge a meeting; i.e. really delete those
 *	      deleted transaction.  This program is linked to a server
 *	      so it can use the privileged procedure of create_mtg, and
 *	      the like.
 *
 */

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#include <discuss/types.h>
#include <discuss/dsc_et.h>
#include <discuss/tfile.h>
#include <discuss/interface.h>
#include <discuss/acl.h>
#include "mtg.h"

#define NULL 0
#define MAX_TRNS 1000
#define min(a, b) (a < b ? a : b)

static int tempf;
static char *mtg_name = NULL, *location = NULL, *chairman = NULL, *trn_file = NULL;
static char *backup_location = NULL;
static char *future_location = NULL;
static int found_eof = 0;
static int error_occurred = 0;

tfile unix_tfile ();
char *malloc();

extern char rpc_caller[];
extern int has_privs;
extern int errno;
extern int no_nuke;

main (argc, argv)
int argc;
char **argv;
{
     int i,n,low,high;
     mtg_info old_mtg_info,new_mtg_info;
     trn_info old_trn_info;
     int result;
     dsc_acl *acl_list,*new_acl_list;
     dsc_acl_entry *ae;
     char *new_modes;
     tfile tf;
     char control_name[256];
     int control_fd;
     
     init_dsc_err_tbl();

     for (i = 1; i < argc; i++) {
	  if (*argv[i] == '-') switch (argv[i][1]) {
	  case 'c':
	       if (++i < argc)
		    chairman = argv[i];
	       continue;

	  case 'n':
	       if (++i < argc)
		    mtg_name = argv[i];
	       continue;

	  default:
	       goto lusage;
	  }
	  if (location == NULL)
	       location = argv[i];
	  else goto lusage;
     }

     if (location == NULL)
	  goto lusage;					/* required */

     has_privs = TRUE;					/* Tell discuss we're special */
     strcpy (rpc_caller, "expunger");

     /* First, we get the mtg info to make sure it exists */
     get_mtg_info (location, &old_mtg_info, &result);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting mtg info\n", location, error_message(result));
	  exit (1);
     }

     get_acl (location, &result, &acl_list);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting acl\n", location, error_message(result));
	  exit (1);
     }

     /* Create the new meeting */
     backup_location = malloc (strlen(location)+5);	/* be generous */
     strcpy (backup_location, location);
     strcat (backup_location, "~");

     future_location = malloc (strlen(location)+5);	/* be generous */
     strcpy (future_location, location);
     strcat (future_location, "#");

     printf("Creating new meeting\n");
     fflush(stdout);

     if (mtg_name == NULL) {
	  mtg_name = old_mtg_info.long_name;
     }
     if (chairman == NULL) {
	  chairman = old_mtg_info.chairman;
     }

     /* get acl's on old meeting, so we can make it new one */
     get_acl (location, &result, &new_acl_list);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting acl\n", backup_location, error_message(result));
	  exit (1);
     }

     create_mtg_priv (backup_location, mtg_name, old_mtg_info.public_flag,
		      old_mtg_info.date_created, chairman,
		      new_acl_list, &result);
     if (result != 0) {
	  fprintf (stderr, "%s: %s while creating new meeting\n",
		   location, error_message(result));
	  exit (1);
     }
     
     /* now, do the actual expunging */
     low = old_mtg_info.lowest;
     high = old_mtg_info.highest;
     create_temp ();

expunge_range:
     for (i = low; i <= high; i++) {
	  get_trn_info (location, i, &old_trn_info, &result);
	  if (result != 0 && result != DELETED_TRN && result != EXPUNGED_TRN) {
	       fprintf(stderr,
		       "Error getting info for transaction [%04d]: %s\n",
		       i, error_message(result));
	       error_occurred = TRUE;
	  } else if (result != 0) {		/* expunge it */
	       no_nuke = TRUE;
	       printf("Expunging transaction [%04d]\n", i);
	       expunge_trn (backup_location, i, &result);
	       no_nuke = FALSE;
	       if (result != 0) {
		    fprintf(stderr,
			    "Error expunging transaction [%04d]: %s\n",
			    i, error_message(result));
		    error_occurred = TRUE;
	       }
	  } else if (result == 0) {
	       ftruncate(tempf,0);
	       lseek(tempf,0,0);
	       tf = unix_tfile (tempf);

	       get_trn (location, i, tf, &result);
	       if (result != 0) {
		    fprintf(stderr, "Error getting transaction [%04d]: %s\n",
			    i, error_message(result));
		    error_occurred = TRUE;
		    free(old_trn_info. author);
		    free(old_trn_info.subject);
		    continue;
	       }

	       tdestroy (tf);
	       lseek(tempf,0,0);

	       tf = unix_tfile (tempf);
	       no_nuke = TRUE;
	       add_trn_priv (backup_location, tf, old_trn_info.subject,
			     old_trn_info.pref, old_trn_info.current,
			     old_trn_info.author, old_trn_info.date_entered,
			     &n, &result);
	       no_nuke = FALSE;
	       if (result != 0) {
		    fprintf(stderr,
			    "Error getting info for transaction %d: %s\n", i,
			    error_message(result));
		    error_occurred = TRUE;
	       }
	       free(old_trn_info.author);
	       free(old_trn_info.subject);
	  }
     }

     /* Check if any new transactions have been added */
     free(old_mtg_info.long_name);
     free(old_mtg_info.chairman);
     free(old_mtg_info.location);
     get_mtg_info(location, &old_mtg_info, &result);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting mtg info\n", location, error_message(result));
	  error_occurred = TRUE;
     } else if (old_mtg_info.highest > high) {		/* New transactions added */
	  low = high+1;
	  high = old_mtg_info.highest;
	  goto expunge_range;
     }

     strcpy(control_name, location);
     strcat(control_name, "/control");
     if ((control_fd = open (control_name, O_RDWR, 0700)) < 0) {
	  error_occurred = TRUE;
     } else {
	  flock(control_fd, LOCK_EX);		/* Hold meeting by the balls */
	  free(old_mtg_info.long_name);
	  free(old_mtg_info.chairman);
	  free(old_mtg_info.location);

	  no_nuke = TRUE;
	  get_mtg_info(location, &old_mtg_info, &result);
	  if (result != 0) {	       
	       fprintf(stderr, "%s: %s while getting mtg info\n", location, error_message(result));
	       error_occurred = TRUE;
	       flock(control_fd,LOCK_UN);
	       close(control_fd);
	  } else if (old_mtg_info.highest > high) {		/* New transactions added */
	       low = high + 1;
	       high = old_mtg_info.highest;
	       flock(control_fd,LOCK_UN);
	       close(control_fd);
	       goto expunge_range;
	  }
     }

     /* When we get here, we have the old meeting locked.  Now we do the move
	as atomically as we can */
     if (!error_occurred) {
	  if (rename(location, future_location) < 0) {
	       perror("rename of old meeting failed");
	       exit (1);
	  }
	  if (rename(backup_location, location) < 0) {
	       perror("rename of new meeting");
	       exit (1);
	  }
	  remove_mtg (future_location, &result);
	  if (result != 0) {
	       fprintf(stderr, "%s: %s while removing new meeting.\n",
		       location, error_message(result));
	       exit (1);
	  }
     } else exit (1);				/* error occurred */

     exit (0);

lusage:
     fprintf(stderr, "usage: expunge mtg_location {-c chairman} {-n name}\n");
     exit (1);
}

/*
 *
 * create_temp () -- Create temp file, and let it be tempf.
 *
 */
create_temp()
{
     char filename [20];

     strcpy (filename, "/tmp/rcXXXXXX");
     mktemp (filename);

     tempf = open (filename, O_RDWR | O_CREAT, 0700);
     if (tempf < 0) {
	  fprintf (stderr, "Cannot open temp file\n");
	  exit (1);
     }
}


