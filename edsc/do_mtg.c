/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * do_mtg.c -- Routines to implement the various lisp requests related
 * 		to meeting manipulation.
 *
 */

#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#include <strings.h>
#include <sys/wait.h>
#include <ctype.h>
#include <sys/time.h>
#include <netdb.h>
#include "errno.h"
#include "rpc_et.h"
#include "edsc.h"

name_blk	*meetings_list = NULL;
int		meetings_list_count;

do_gmi(args)
char *args;
{
     char *cp = args, *mtg_name, delim;
     name_blk nb;
     mtg_info m_info;
     char mtime[30];
     int code;
     struct cache_meeting	*curr;

     /* First, we get the meeting name */
     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

#ifdef EDSC_CACHE
     if (curr = search_cache_meetings(&nb)) {
	     /*
	      * The meeting is already cached, but we'll refresh the cache
	      * information because we don't know if it's up to date.
	      */
	     dsc_get_mtg_info(&nb,&m_info,&code);
	     if (code != 0) {
		     while (code == MTG_MOVED)
			     handle_moved_meeting(&nb, &m_info, &code,
						  nb.aliases[0], TRUE);
		     if (code != 0) {
			     printf(";%s\n", error_message(code));
			     dsc_destroy_name_blk(&nb);
			     return;
		     }
	     }
	     if (curr->m_info.last != m_info.last)
		     cache_it(&nb, curr->m_info.last);
	     dsc_destroy_mtg_info(&curr->m_info);
	     curr->m_info = m_info;
	     curr->meeting_code = code;
     } else {
	     /*
	      * The meeting isn't cached yet; just pull it into the
	      * cache and we'll use the meeting information from there.
	      */
	     curr = get_cache_meeting(&nb);
	     curr->ref_count--;
	     m_info = curr->m_info;
	     if (curr->meeting_code != 0) {
		     printf(";%s\n", error_message(curr->meeting_code));
		     dsc_destroy_name_blk(&nb);
		     return;
	     }
     }
     
#else
     dsc_get_mtg_info(&nb,&m_info,&code);
     if (code != 0) {
	  while (code == MTG_MOVED)
		  handle_moved_meeting(&nb, &m_info, &code,
				       nb.aliases[0], TRUE);
	  if (code != 0) {
		  printf(";%s\n", error_message(code));
		  dsc_destroy_name_blk(&nb);
		  return;
	  }
     }
#endif

     strcpy(mtime, short_time(&m_info.date_created));
     printf("(\"%s\" \"%s\" \"%s\" %d %d %d %d \"%s\" \"%s\" %d \"%s\" %d)\n",
	    m_info.location,
	    m_info.long_name,
	    m_info.chairman,
	    m_info.first,
	    m_info.last,
	    m_info.lowest,
	    m_info.highest,
	    mtime,
	    short_time(&m_info.date_modified),
	    m_info.public_flag,
	    m_info.access_modes,
	    nb.last);
     
     dsc_destroy_name_blk(&nb);
#ifndef EDSC_CACHE
     dsc_destroy_mtg_info(&m_info);
#endif
}

handle_moved_meeting(nb, m_info, code, name, update)
	name_blk *nb;
	mtg_info *m_info;
	int *code;
	char *name;
	int update;
{
	/* Meeting has moved.  In this case, dsc_public.m_info.long_name
	 * should contain the hostname/pathname for the meeting.
	 * We should update our information to reflect this change.
	 */

	char	*old_hostname, *old_pathname, *new_host, *new_path;
	int	dummy;

	old_hostname = nb->hostname;
	old_pathname = nb->pathname;
	
	nb->hostname = new_host = malloc(strlen(m_info->long_name)+1);
	strcpy(nb->hostname, m_info->long_name);
	
	nb->pathname = new_path = malloc(strlen(m_info->location)+1);
	strcpy(nb->pathname, m_info->location);
	
	dsc_get_mtg_info(nb, m_info, code);
	if (*code != 0 && *code != MTG_MOVED) {
		printf(";Error checking moved meeting %s", name);
	} else {
		printf("-Warning: %s moved to %s:%s\n",
			name, new_host, new_path);
		if (update) {
			/* Delete old meeting */
			nb->hostname = old_hostname;
			nb->pathname = old_pathname;
			nb->status |= DSC_ST_DELETED;
			dsc_update_mtg_set(user_id, nb, 1, &dummy);
			nb->status &= ~(DSC_ST_DELETED);
			free(nb->hostname);
			nb->hostname = new_host;
			free(nb->pathname);
			nb->pathname = new_path;
			dsc_update_mtg_set(user_id, nb, 1, &dummy);
		} else {
			free(old_hostname);
			free(old_pathname);
		}
	}
}


#define MAX_ERR_LIST	20

do_gml(args)
char *args;
{
     name_blk *nbp;
     char **aliases;
     int code, i;
     int first;
     bool updated;

     if (meetings_list)
	     dsc_destroy_mtg_set(meetings_list, meetings_list_count);
     dsc_expand_mtg_set(user_id, "*", &meetings_list, &meetings_list_count,
			&code); 
     if (code) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     set_warning_hook(bit_bucket);
     putchar('(');

     first = TRUE;
     for (i = 0; i < meetings_list_count; i++) {
	  nbp = &meetings_list[i];

	  dsc_updated_mtg(nbp, &updated, &code);
	  if (first) {
	       first = FALSE;
	  } else
	       putchar(' ');

	  if (code == NO_SUCH_TRN) {
		  printf("(1");
	  } else if (code) {
		  if ((code == RPC_HOST_UNKNOWN) || (code == ETIMEDOUT) ||
		      (code == ECONNREFUSED) || (code == ECONNRESET) ||
		      (code == ENETDOWN) || (code == ENETUNREACH))
			  printf("(\"%s: %s\"", error_message(code),
				 nbp->hostname);
		  else
			  printf("(\"%s\"", error_message(code));
		  code = 0;
	  } else
		  printf("(%d", updated ? 1 : 0);
	  aliases = nbp -> aliases;
	  while (*aliases) {
	       printf(" \"%s\"", *(aliases++));
	  }

	  printf(")");
     }

     printf(")\n");
}

do_ss(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     int code;

     /* First, we get the transaction number */
     if (get_word(&cp, &trn_string, " ", &delim) < 0) {
	  printf(";Missing trn number\n");
	  return;
     }

     if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
	  printf(";Missing meeting name\n");
	  return;
     }

     trn_num = atoi(trn_string);
     dsc_get_mtg (user_id, mtg_name, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }

     nb.last = trn_num;
     dsc_update_mtg_set (user_id, &nb, 1, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  dsc_destroy_name_blk(&nb);
	  return;
     }
     dsc_destroy_name_blk(&nb);
     printf("()\n");
}

/*
 * Get meeting number
 */
do_gmn(args)
	char	*args;
{
	char	*cp = args, *mtg_name, delim;
	
	if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
		printf(";Missing meeting name\n");
		return;
	}

}	

do_am(args)
	char	*args;
{
     char *cp = args, delim, *host, *path;
     char **aliases;
     name_blk 	nb, temp_nb;
     int	code,j;

     if (get_word(&cp, &host, " ", &delim) < 0) {
	  printf(";Missing hostname\n");
	  return;
     }

     if (get_word(&cp, &path, ")", &delim) < 0) {
	  printf(";Missing pathname\n");
	  return;
     }
     hostpath_to_nb (host, path, &nb, &code);
     if (code != 0) {
	  printf(";%s\n", error_message(code));
	  return;
     }
     for (j = 0; nb.aliases[j] != NULL; j++) {
	     dsc_get_mtg (user_id, nb.aliases[j], &temp_nb, &code);
	     if (code == 0) {
		     printf(";Meeting %s already exists.\n", nb.aliases[j]);
		     dsc_destroy_name_blk(&nb);
		     return;
	     }
     }
     dsc_update_mtg_set(user_id, &nb, 1, &code);
     if (code) {
	     dsc_destroy_name_blk(&nb);
	     printf(";%s\n", error_message(code));
	     return;
     }

     printf("(1 ");
     aliases = nb.aliases;
     while (*aliases) {
	     printf(" \"%s\"", *(aliases++));
     }
     printf(")\n");
     dsc_destroy_name_blk(&nb);

     /*
      * Update the meetings list.
      */
     if (meetings_list)
	     dsc_destroy_mtg_set(meetings_list, meetings_list_count);
     dsc_expand_mtg_set(user_id, "*", &meetings_list, &meetings_list_count,
			&code); 
     if (code) {
	     meetings_list = NULL;
     }
}

do_dm(args)
	char	*args;
{
	char *cp = args, delim, *mtg_name;
	name_blk 	nb;
	int	code;
	
	/*
	 * Parse out the meeting name
	 */
	if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
		printf(";Missing meeting name\n");
		return;
	}
	dsc_get_mtg (user_id, mtg_name, &nb, &code);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		return;
	}

	nb.status |= DSC_ST_DELETED;
	dsc_update_mtg_set(user_id, &nb, 1, &code);
	dsc_destroy_name_blk(&nb);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		return;
	}
	printf("(\"%s\")\n", mtg_name);
}

/*
 * Utility subroutines go here...
 */

hostpath_to_nb (host, path, nbp, code)
char *host, *path;
name_blk *nbp;
int *code;
{
     struct hostent *hp;
     mtg_info m_info;
     char *short_name;

     nbp -> hostname = nbp -> pathname = nbp -> user_id = nbp -> spare = NULL;
     nbp -> date_attended = nbp -> last = nbp -> status = 0;

     hp = gethostbyname (host);
     if (hp != NULL)
	  host = hp -> h_name;			/* use canonical if possible */
     nbp -> hostname = malloc((unsigned)strlen(host)+1);
     strcpy(nbp -> hostname,host);
     nbp -> pathname = malloc((unsigned)strlen(path)+1);
     strcpy(nbp -> pathname, path);
     nbp -> user_id = malloc((unsigned)strlen(user_id)+1);
     strcpy(nbp -> user_id, user_id);
     nbp -> aliases = (char **)NULL;
     dsc_get_mtg_info(nbp, &m_info, code);
     short_name = rindex(path,'/');
     if (!short_name)
	  short_name = rindex(path,':');
     while (*code == MTG_MOVED)
	     handle_moved_meeting(nbp, &m_info, code, short_name, FALSE);
     if (*code) {
	  if (*code == NO_ACCESS) {
	       *code = CANT_ATTEND;		/* friendlier error msg */
	  }
	  goto punt;
     }
     nbp -> aliases = (char **)calloc(3, sizeof(char *));
     nbp -> aliases[0] = malloc(strlen(m_info.long_name)+1);
     strcpy(nbp -> aliases[0], m_info.long_name);
     nbp -> aliases[1] = malloc(strlen(short_name));
     strcpy(nbp -> aliases[1],short_name+1);
     nbp -> aliases[2] = (char *)NULL;
     *(nbp->spare = malloc(1)) = '\0';
     free(m_info.location);
     free(m_info.chairman);
     free(m_info.long_name);

     return;

punt:
     dsc_destroy_name_blk (nbp);
     return;
}
