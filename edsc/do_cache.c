/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */

/*
 *
 * do_cache.c -- Routines to implement transaction caching/look ahead
 *
 * 	(Not yet working --- when it is, it will replace the emacs
 * lisp code, and things will be Much Faster.)
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#include <ctype.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <stdio.h>

#include "edsc.h"

#ifdef EDSC_CACHE

int	cache_working;

struct cache_info *top_used, *bot_used, *top_empty;

struct cache_info *cache;
int	cachesize;

struct cache_meeting *cache_meetings;

char			cache_directory[20];
cache_dir     		cache_strategy[CACHE_STRATEGY_SIZE];
int			cache_pc;
struct cache_info	*cache_current, *cache_ptr;
cache_dir		cache_current_direction;

cache_dir		cache_default_strategy[] = {
	CACHE_OP_GET_CUR,
	CACHE_OP_FETCH_CUR | CACHE_DIR_NEXT,
	CACHE_OP_FETCH_CUR | CACHE_DIR_NREF,
	CACHE_OP_FETCH_CUR | CACHE_DIR_PREV,
	CACHE_OP_FETCH_CUR | CACHE_DIR_PREF,
	CACHE_OP_FETCH_CUR | CACHE_DIR_FREF,
	CACHE_OP_FETCH_CUR | CACHE_DIR_LREF,
	CACHE_OP_SET_PTR,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_MFETCH_PTR | CACHE_DIR_CURRENT,
	CACHE_OP_STOP
	};

/*
 * This is called from the main loop of discuss.  We don't return
 * until we see that there is pending input on stdin.
 */
void do_cache_work()
{
	cache_dir	ins;
	int		skip_poll; /* If true, don't poll */
	int		retval;
	int		next_trn; /* Next transaction to get */
	struct cache_info	*new_ptr;
	
	skip_poll = 1;
	while (skip_poll || !poll_input(1)) {
		skip_poll = 0;
		ins = cache_strategy[cache_pc];
#ifdef CACHE_DEBUG
		fprintf(stderr, "- Cache instruction: %02x\n", ins);
#endif
		switch (ins & CACHE_OP_MASK) {
		case CACHE_OP_STOP:
			cache_working = 0;
			return;
		case CACHE_OP_GET_CUR:
			retval = cache_transaction(&cache_current->meeting->nb,
						   cache_current->trn_num,
						   NULL);
			if (retval) {
				if (retval == -1)
					skip_poll++;
			} else
				cache_pc--;
			break;
		case CACHE_OP_FETCH_CUR:
			next_trn = move_trn(cache_current, ins&CACHE_DIR_MASK);
			if (!next_trn) {
				skip_poll++;
				break;
			}
			retval = cache_transaction(&cache_current->meeting->nb,
						   next_trn, NULL);
			if (retval) {
				if (retval == -1)
					skip_poll++;
			} else
				cache_pc--;
			break;
		case CACHE_OP_SET_PTR:
			cache_ptr = cache_current;
			skip_poll++;
			break;
		case CACHE_OP_MFETCH_PTR:
			if (!cache_ptr) {
				skip_poll++;
				break;
			}
			next_trn = move_trn(cache_ptr, ins & CACHE_DIR_MASK);
			if (!next_trn) {
				skip_poll++;
				break;
			}
			retval = cache_transaction(&cache_ptr->meeting->nb,
						   next_trn,
						   &new_ptr);
			if (retval) {
				cache_ptr = new_ptr;
				if (retval == -1)
					skip_poll++;
			} else
				cache_pc--;
			break;
		}
		cache_pc++;
	}
}

int move_trn(entry, dir)
	struct cache_info	*entry;
	cache_dir		dir;
{
	if (!(entry->flags & CACHE_FLG_INFO) || entry->info_code)
		return(0);
	if (dir == CACHE_DIR_CURRENT)
		dir = cache_current_direction;
	switch (dir) {
	case CACHE_DIR_NEXT:
		return(entry->t_info.next);
	case CACHE_DIR_PREV:
		return(entry->t_info.prev);
	case CACHE_DIR_NREF:
		return(entry->t_info.nref);
	case CACHE_DIR_PREF:
		return(entry->t_info.pref);
	case CACHE_DIR_FREF:
		return(entry->t_info.fref);
	case CACHE_DIR_LREF:
		return(entry->t_info.lref);
	}
	return(0);
}

/*
 * Set the current direction....
 */
do_scd(args)
char *args;
{
     char *cp = args, delim, *dir_string;
     int	newdir;
     

     /* we get the direction */
     if (get_word(&cp, &dir_string, ")", &delim) < 0) {
	  printf(";Missing direction\n");
	  return;
     }
     newdir = atoi(dir_string);
     if ((newdir > 0) && (newdir <= 6))
	     cache_current_direction = newdir;
     printf("()\n");
}

/*
 * Invalidate transaction 
 */
do_it(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     int	code;

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
     cache_it(&nb, trn_num);
     printf("()\n");
}

cache_it(nbp, trn_num)
     name_blk *nbp;
     trn_nums trn_num;
{
     struct	cache_info	*entry;

     if (entry = cache_search(nbp, trn_num)) {
	     if (entry->flags & CACHE_FLG_INFO)
		     dsc_destroy_trn_info3(&entry->t_info);
	     entry->flags &= ~(CACHE_FLG_INFO|CACHE_FLG_TEXT);
	     entry->info_code = entry->text_code = 0;
     }
}

/*
 * Invalidate transaction and neighbors
 */
do_itn(args)
char *args;
{
     char *cp = args, *mtg_name, delim, *trn_string;
     trn_nums trn_num;
     name_blk nb;
     int	code;

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
     cache_itn(&nb, trn_num);
     printf("()\n");
}

cache_itn(nbp, trn_num)
     name_blk *nbp;
     trn_nums trn_num;
{
     struct	cache_info	*current;
	     

     if (!(current = cache_search(nbp, trn_num))) {
	     while (!current || !(current->flags & CACHE_FLG_INFO))
		     cache_transaction(nbp, trn_num, &current);
     }
     cache_it(nbp, current->t_info.next);
     cache_it(nbp, current->t_info.prev);
     cache_it(nbp, current->t_info.nref);
     cache_it(nbp, current->t_info.pref);
     cache_it(nbp, current->t_info.fref);
     cache_it(nbp, current->t_info.lref);
     if (current->flags & CACHE_FLG_INFO)
	     dsc_destroy_trn_info3(&current->t_info);
     current->info_code = current->text_code = 0;
     current->flags &= ~(CACHE_FLG_INFO|CACHE_FLG_TEXT);	     
}
	
/*
 * Cache flush meeting --- flush the cache of all transactions from a meeting
 */
do_im(args)
char *args;
{
     char *cp = args, *mtg_name, delim;
     name_blk nb;
     int code;
     struct cache_meeting	*meeting;
     struct cache_info *curr = top_used, *next;

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

     if (meeting = search_cache_meetings(&nb)) {
	     while (curr) {
		     next = curr->next;
		     if (curr->meeting == meeting) {
			     if (curr->prev)
				     curr->prev->next = curr->next;
			     else
				     top_used = curr->next;
			     if (curr->next)
				     curr->next->prev = curr->prev;
			     else
				     bot_used = curr->prev;
			     free_cache_entry(curr);
			     curr->next = top_empty;
			     top_empty = curr;
		     }
		     curr = next;
	     }
     }
     dsc_destroy_name_blk(&nb);
     printf("()\n");
}


/*
 * Suck a transaction into the cache.  Returns true when the
 * transaction has been entered into the cache.  If it returns false,
 * the cache_transaction should be called again with the same arguments.
 */
int cache_transaction(nbp, trn_num, trn_entry)
	name_blk	*nbp;
	trn_nums trn_num;
	struct cache_info **trn_entry;
{
	struct	cache_info	*entry, *link;
	trn_nums		trn;
	cache_dir		i;

	entry = cache_search(nbp, trn_num);
#ifdef CACHE_DEBUG
	fprintf(stderr, "- cache_transaction(%d)\n", trn_num);
#endif
	if (!entry) {
		entry = cache_empty();
		entry->meeting = get_cache_meeting(nbp);
		entry->trn_num = trn_num;
	}
	if (trn_entry)
		*trn_entry = entry;
	if (!(entry->flags & CACHE_FLG_INFO)) {
		dsc_get_trn_info3(&entry->meeting->nb, entry->trn_num,
				  &entry->t_info, &entry->info_code);
		entry->flags |= CACHE_FLG_INFO;
		if (!entry->info_code) {
			for (i=CACHE_DIR_NEXT; i <= CACHE_DIR_PREF; i++) {
				if ((trn = move_trn(entry, i)) &&
				    (link = cache_search(nbp, trn)) &&
				    (link->flags & CACHE_FLG_INFO) &&
				    (move_trn(link, ((i-1)^1)+1) != trn))
					cache_it(nbp, trn);
			}
		}
		return (entry->flags & CACHE_FLG_TEXT);
	}
	if (!(entry->flags & CACHE_FLG_TEXT)) {
		cache_get_transaction_text(entry);
		return(1);
	}
	return(-1);
}

void cache_get_transaction_text(entry)
	struct cache_info	*entry;
{
	trn_info3 tinfo;
	tfile tf;
	int fd;
	char *plural;
	char line[255];
	int code;
	
	/*
	 * Flag the fact that we tried to get the transaction text.
	 */
	entry->flags |= CACHE_FLG_TEXT;
	
	/*
	 * Find a filename that's not in use and read the
	 * transaction text into that file.
	 */
	sprintf(entry->filename, "%s/edsc-%d-XXXXXX", cache_directory,
		entry->trn_num);
	if ((fd = mkstemp(entry->filename)) < 0) {
		entry->text_code = errno;
		return;
	}	

	tf = unix_tfile(fd);

	tinfo = entry->t_info;
	
	if (tinfo.num_lines != 1)
		plural = "s";
	else
		plural = "";
     
	if (tinfo.subject [0] == '\0')
		tinfo.num_lines--;

	if (tinfo.signature && strcmp(tinfo.signature, tinfo.author))
		(void) sprintf (line, "[%04d]%c %s (%s) %s %s (%d line%s)\n",
				tinfo.current,
				tinfo.flags & TRN_FLAG1 ? 'F' : ' ',
				tinfo.author, tinfo.signature,
				entry->meeting->m_info.long_name,
				short_time (&tinfo.date_entered),
				tinfo.num_lines, plural);
	else
		(void) sprintf (line, "[%04d]%c %s %s %s (%d line%s)\n",
				tinfo.current,
				tinfo.flags & TRN_FLAG1 ? 'F' : ' ',
				tinfo.author,
				entry->meeting->m_info.long_name,
				short_time (&tinfo.date_entered),
				tinfo.num_lines, plural);
	twrite (tf, line, strlen (line), &code);
	if (tinfo.subject [0] != '\0') {
		twrite (tf, "Subject: ", 9, &code);
		twrite (tf, tinfo.subject, strlen (tinfo.subject), &code);
		twrite (tf, "\n", 1, &code);
	}

	dsc_get_trn(&entry->meeting->nb, entry->trn_num, tf, &code);
	if (code != 0) {
		entry->text_code = code;
		return;
	}

	if (tinfo.pref == 0 && tinfo.nref == 0)
		(void) sprintf (line, "--[%04d]--\n", tinfo.current);
	else if (tinfo.pref == 0)
		(void) sprintf (line, "--[%04d]-- (nref = [%04d])\n",
				tinfo.current, tinfo.nref);
	else if (tinfo.nref == 0)
		(void) sprintf (line, "--[%04d]-- (pref = [%04d])\n",
				tinfo.current, tinfo.pref);
	else
		(void) sprintf (line,
				"--[%04d]-- (pref = [%04d], nref = [%04d])\n",
				tinfo.current, tinfo.pref, tinfo.nref);
	twrite (tf, line, strlen (line), &code);
	
	tclose(tf, &code);
	(void) close(fd);
	if (code != 0) {
		entry->text_code = code;
		return;
	}
}

/*
 * Initialize the cache
 */
void cache_init(size)
	int	size;
{
	int	i;
	
	if (cache)
		cache_flush();
	if (!size)
		size = CACHE_DEFAULT_SIZE;
	if (cachesize && (cachesize != size))
		return;
	if (cache)
		free(cache);
	cache = (struct cache_info *) malloc(sizeof(struct cache_info)*size);
	memset(cache, 0, sizeof(struct cache_info)*size);
	top_used = bot_used = NULL;
	top_empty = cache;
	for (i = 1; i < size; i++)
		cache[i-1].next = cache+i;
	cache[size-1].next = NULL;
	cachesize = size;

	i = 0;
	while (1) {
		if ((cache_strategy[i] = cache_default_strategy[i])
		    == CACHE_OP_STOP)
			break;
		i++;
	}
	cache_current_direction = CACHE_DIR_NEXT;
try_again:
	sprintf(cache_directory, "%s/.edscXXXXXX", CACHE_DIR);
	mktemp(cache_directory);
	if (mkdir(cache_directory, 0711)) {
		if (errno == EEXIST)
			goto try_again;
		printf("; Couldn't create cache directory %s! --- %s\n",
		       cache_directory, error_message(errno));
		exit(1);
	}
}

/*
 * Flush the cache
 */
void cache_flush()
{
	struct cache_info *curr = top_used;
	struct cache_info *next = NULL;

	while (curr) {
		free_cache_entry(curr);
		next = curr->next;
		curr->next = top_empty;
		top_empty = curr;
		curr = next;
	}
	top_used = bot_used = NULL;
}

/*
 * Cache shutdown
 */
cache_shutdown()
{
	cache_flush();
	(void) rmdir(cache_directory);
}

/*
 * Free a cache entry
 */
void free_cache_entry(entry)
	struct cache_info	*entry;
{
	if (entry->filename[0])
		unlink(entry->filename);
	entry->filename[0]='\0';
	if (entry->flags & CACHE_FLG_INFO)
		dsc_destroy_trn_info3(&entry->t_info);
	entry->flags = 0;
	if (!(--entry->meeting->ref_count))
		free_cache_meeting(entry->meeting);
}

/*
 * Find empty cache entry, flushing an old entry if necessary....
 * Splice new entry into the top of the LRU list.
 */
struct cache_info *cache_empty()
{
	struct cache_info	*empty;
	
	if (top_empty) {
		empty = top_empty;
		top_empty = top_empty->next;
	} else {
		empty = bot_used;
		bot_used = bot_used->prev;
		bot_used->next = NULL;
		free_cache_entry(empty);
	}
	if (top_used)
		top_used->prev = empty;
	else
		bot_used = empty;
	empty->next= top_used;
	empty->prev=NULL;
	top_used = empty;
	empty->flags = 0;
	return(empty);
}

/*
 * Search cache, placing found cache at the top of the LRU list
 */
struct cache_info *cache_search(nbp, trn)
	name_blk	*nbp;
	trn_nums trn;
{
	struct cache_info	*curr = top_used;
	
	while (curr) {
		if ((trn == curr->trn_num) &&
		    !strcasecmp(nbp->hostname, curr->meeting->nb.hostname) &&
		    !strcasecmp(nbp->pathname, curr->meeting->nb.pathname))
			break;
		curr = curr->next;
	}
	if (curr) {
		if (curr->prev) {
			/* move cache entry to the top */
			curr->prev->next = curr->next;
			if (curr->next)
				curr->next->prev = curr->prev;
			else
				bot_used = curr->prev;
			
			top_used->prev = curr;
			curr->next = top_used;
			
			curr->prev = NULL;
			top_used = curr;
		}
		return(curr);
	} else
		return(NULL);
}

/*
 * Cache meeting subroutines
 */

/*
 * Get the cache_meeting structure for a meeting.  If it doesn't
 * exist, create a cache_meeting structure for it.
 */
struct cache_meeting *get_cache_meeting(nbp)
	name_blk	*nbp;
{
	struct cache_meeting	*curr;
	int			code;
	
	for (curr=cache_meetings; curr; curr = curr->next) {
		if (!strcasecmp(nbp->hostname, curr->nb.hostname) &&
		    !strcasecmp(nbp->pathname, curr->nb.pathname))
			break;
	}
	if (!curr) {
		curr = (struct cache_meeting *)
			malloc(sizeof(struct cache_meeting));
		if (!curr) {
			printf("; Abort!  malloc for cache_meeting failed!\n");
			abort();
		}
		curr->ref_count = 0;
		dsc_copy_name_blk(nbp, &curr->nb);
		dsc_get_mtg_info(nbp, &curr->m_info, &code);
		while (code == MTG_MOVED)
			handle_moved_meeting(&curr->nb, &curr->m_info, &code,
					     curr->nb.aliases[0], TRUE);
		curr->meeting_code = code;
		curr->next = cache_meetings;
		curr->prev = NULL;
		if (cache_meetings)
			cache_meetings->prev = curr;
		cache_meetings = curr;
	}
	curr->ref_count++;
	return(curr);
}

struct cache_meeting *search_cache_meetings(nbp)
	name_blk	*nbp;
{
	struct cache_meeting	*curr;
	
	for (curr=cache_meetings; curr; curr = curr->next) {
		if (!strcasecmp(nbp->hostname, curr->nb.hostname) &&
		    !strcasecmp(nbp->pathname, curr->nb.pathname))
			return(curr);
	}
	return(NULL);
}


/*
 * Free a cache meeting
 */
void free_cache_meeting(meeting)
	struct cache_meeting	*meeting;
{
	/*
	 * Split around the cache meeting information
	 */	
	if (meeting->prev)
		meeting->prev->next = meeting->next;
	else
		cache_meetings = meeting->next;
	if (meeting->next)
		meeting->next->prev = meeting->prev;
	dsc_destroy_name_blk(&meeting->nb);
	dsc_destroy_mtg_info(&meeting->m_info);
	free(meeting);
}

/* Procedure to check for waiting input on a file descriptor.
 * Does not actually read the input.
 */

int poll_input(f)
	int f;
{
	struct timeval	timeout;
	fd_set		readfds;

#ifdef CACHE_DEBUG
	fprintf(stderr, "(Polling)\n");
#endif
	FD_ZERO(&readfds);
	FD_SET(f, &readfds);
	timeout.tv_sec = 0;
	timeout.tv_usec = 0;
	return(select(f+1, &readfds, 0, 0, &timeout));
}

#endif
