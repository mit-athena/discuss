/*
 *
 * expunge -- program to expunge a meeting; i.e. really delete those
 *	      deleted transaction.  This program is linked to a server
 *	      so it can use the privileged procedure of create_mtg, and
 *	      the like.
 *
 */
#include "../include/types.h"
#include "../include/dsc_et.h"
#include "../include/tfile.h"
#include "../include/interface.h"
#include "../include/acl.h"
#include "mtg.h"

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#define NULL 0
#define MAX_TRNS 1000
#define BUFFER_SIZE 500000
#define min(a, b) (a < b ? a : b)

static int tempf;
static char *mtg_name = NULL, *location = NULL, *chairman = NULL, *trn_file = NULL;
static char *backup_location = NULL;
static int found_eof = 0;
static int error_occurred = 0;

static char tran_buf[BUFFER_SIZE];

tfile unix_tfile ();
char *malloc();

extern rpc_caller[];
extern int has_privs;
extern int errno;

main (argc, argv)
int argc;
char **argv;
{
     int i,n;
     mtg_info old_mtg_info,new_mtg_info;
     trn_info old_trn_info;
     int result;
     Acl *acl_list,*new_acl_list;
     acl_entry *ae;
     char *new_modes;
     tfile tf;
     
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

     /* Before futzing with things, rename the meeting */
     backup_location = malloc (strlen(location)+5);	/* be generous */
     strcpy (backup_location, location);
     strcat (backup_location, "~");

     printf("Renaming meeting\n");
     fflush(stdout);

     if (rename (location, backup_location) < 0) {
	  fprintf (stderr, "%s: %s while renaming meeting\n", location, error_message(errno));
	  exit (1);
     }

     /* try to get mtg_info again, just because of caching open descriptors */
     get_mtg_info (backup_location, &new_mtg_info, &result);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting renamed mtg info\n", backup_location, error_message(result));
	  exit (1);
     }

     if (mtg_name == NULL) {
	  mtg_name = old_mtg_info.long_name;
     }
     if (chairman == NULL) {
	  chairman = old_mtg_info.chairman;
     }
     
     /* Go through old acl, and only give people read access.  This will keep
	concurrent things from losing (people will get access problems) */
     for (ae = acl_list->acl_entries, n = acl_list->acl_length; n; --n, ++ae) {
	  new_modes = (char *)acl_intersection(ae->modes,"rs");
	  set_access(backup_location, ae->principal, new_modes, &result);
	  if (result != 0) {
	       fprintf(stderr, "%s: %s while setting acl\n", backup_location, error_message(result));
	       exit (1);
	  }
	  free(new_modes);
     }
     /* get acl's on old meeting, so we can make it new one */
     get_acl (backup_location, &result, &new_acl_list);
     if (result != 0) {
	  fprintf(stderr, "%s: %s while getting acl\n", backup_location, error_message(result));
	  exit (1);
     }

     create_mtg_priv (location, mtg_name, old_mtg_info.public_flag, old_mtg_info.date_created, chairman, new_acl_list, &result);
     if (result != 0) {
	  fprintf (stderr, "%s: %s while creating new meeting\n", location, error_message(result));
	  /* XXX rename back */
	  exit (1);
     }

     /* now, do the actual expunging */

     create_temp ();

     for (i = old_mtg_info.lowest; i <= old_mtg_info.highest; i++) {
	  get_trn_info (backup_location, i, &old_trn_info, &result);
	  if (result != 0 && result != DELETED_TRN && result != EXPUNGED_TRN) {
	       fprintf(stderr, "Error getting info for transaction [%04d]: %s\n", i, error_message(result));
	       error_occurred = TRUE;
	  } else if (result != 0) {		/* expunge it */
	       printf("Expunging transaction [%04d]\n", i);
	       expunge_trn (location, i, &result);
	       if (result != 0) {
		    fprintf(stderr, "Error expunging transaction [%04d]: %s\n", i, error_message(result));
		    error_occurred = TRUE;
	       }
	  } else if (result == 0) {
	       if (old_trn_info.num_chars > BUFFER_SIZE) {
		    ftruncate(tempf,0);
		    lseek(tempf,0,0);
		    tf = unix_tfile (tempf);
	       } else {
		    tf = mem_tfile (tran_buf, old_trn_info.num_chars);
	       }

	       get_trn (backup_location, i, tf, &result);
	       if (result != 0) {
		    fprintf(stderr, "Error getting transaction [%04d]: %s\n", i, error_message(result));
		    error_occurred = TRUE;
		    free(old_trn_info. author);
		    free(old_trn_info.subject);
		    continue;
	       }

	       tdestroy (tf);

	       if (old_trn_info.num_chars > BUFFER_SIZE) {
		    lseek(tempf,0,0);
		    tf = unix_tfile (tempf);
	       } else {
		    tf = mem_tfile (tran_buf, old_trn_info.num_chars);
	       }

	       add_trn_priv (location, tf, old_trn_info.subject, old_trn_info.pref, old_trn_info.current, old_trn_info.author, old_trn_info.date_entered, &n, &result);
	       if (result != 0) {
		    fprintf(stderr, "Error getting info for transaction %d: %s\n", i, error_message(result));
		    error_occurred = TRUE;
	       }
	       tdestroy (tf);
	       free(old_trn_info.author);
	       free(old_trn_info.subject);
	  }
     }

     /* All done, now reset modes to something useful */
     for (ae = acl_list->acl_entries, n = acl_list->acl_length; n; --n, ++ae) {
	  set_access(location, ae->principal, ae->modes, &result);
	  if (result != 0) {
	       fprintf(stderr, "%s: %s while setting acl entry %s to %s\n", location, ae->principal, ae->modes, error_message(result));
	       exit (1);
	  }
     }

     if (!error_occurred) {
	  remove_mtg (backup_location, &result);
	  if (result != 0) {
	       fprintf(stderr, "%s: %s while removing backup meeting.\n", location, error_message(result));
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


