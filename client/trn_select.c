/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * $Id: trn_select.c,v 1.17 1999-02-08 14:46:55 danw Exp $
 *
 */

#ifndef lint
static char rcsid_discuss_c[] =
    "$Id: trn_select.c,v 1.17 1999-02-08 14:46:55 danw Exp $";
#endif /* lint */

#define	MIN(a,b)	((a)<(b)?(a):(b))
#define	MAX(a,b)	((a)>(b)?(a):(b))
#include <stdio.h>
#include <discuss/discuss.h>
#include "globals.h"

extern char *malloc();

selection_list *
sl_insert_range(low, high, flags, old_list, code_ptr)
	register int low, high;
	int flags;
	register selection_list *old_list;
	int *code_ptr;
{
	register selection_list *p, *new;
	p = old_list;

	new = (selection_list *)malloc(sizeof(selection_list));
	if (!new) {
		*code_ptr = errno;
		return((selection_list *)NULL);
	}
	new->low = low;
	new->high = high;
	new->flags = flags;
	new->next = (selection_list *)NULL;

	if (!p)
		return(new);	/* first one, so just return it */

	/* we just add it on to the end, and let the user lose if he
	   wants...(he might want them printed/listed out of order.
	   Ken, try to document... */

	while (p->next != NULL) {
	     p = p->next;
	}

	p->next = new;
	return(old_list);
}

selection_list *
sl_insert_num(num, flags, old_list, code_ptr)
	int num;
	int flags;
	selection_list *old_list;
	int *code_ptr;
{
	return(sl_insert_range(num, num, flags, old_list, code_ptr));
}

void
sl_free(list)
	selection_list *list;
{
	register selection_list *p, *q;
	p = list;
	while (p) {
		q = p->next;
		free(p);
		p = q;
	}
}

int filter_call_func (func, t, r, flags)
int (*func)();
trn_info3 *t;
int *r;
int flags;
{
  if (flags & filter_ONLY_DELETED) if (!(t->flags & TRN_FDELETED)) return (0);
  if (flags & filter_ONLY_INITIAL) if (t->pref) return (0);
  if (flags & filter_ONLY_TERMINAL) if (t->nref) return (0);
  /* note, FLAG_SET and FLAG_RESET are not really opposites.  
     The default is to map transaction with either flag set or reset */
  if (flags & filter_FLAG_SET) if (!(t->flags & TRN_FLAG1)) return (0);
  if (flags & filter_FLAG_RESET) if (t->flags & TRN_FLAG1) return (0);
  return (func (t, r));
}

int
sl_map(func, list, filter_flags)
int (*func)();		/* return 0 -> continue */
selection_list *list;
int filter_flags;
{
     register selection_list *p;
     register int i;
     int result,trn_no;
     trn_info3 t_info;
     
     flag_interrupts();
     for (p = list; p; p = p->next) {
	  if ((p->flags & flag_AREF)) {			/* Handle aref */
	       dsc_get_trn_info3 (&dsc_public.nb, p->low, &t_info, &result);
	       t_info.current = p->low;
	       if (result != 0) {
		    filter_call_func(func, &t_info, &result, filter_flags);
		    dsc_destroy_trn_info3(&t_info);
		    if (result != 0) {
			 return(result);
		    }
		    continue;
	       }
	       if (t_info.fref != t_info.current) {	/* Get fref */
		    dsc_destroy_trn_info3(&t_info);
		    trn_no = t_info.fref;
		    dsc_get_trn_info3 (&dsc_public.nb, trn_no, &t_info, &result);
		    t_info.current = trn_no;
	       }

	       do {
		    filter_call_func(func, &t_info, &result, filter_flags);
		    dsc_destroy_trn_info3(&t_info);
		    if (result != 0)
			 return(result);

		    if (interrupt)
			 goto exit;

		    trn_no = t_info.nref;
		    if (trn_no != 0) {
			 dsc_get_trn_info3 (&dsc_public.nb, trn_no, &t_info, &result);
			 t_info.current = trn_no;
		    }
	       } while (trn_no != 0);
	  } else if (filter_flags & 
		     (filter_INCLUDE_DELETED | filter_ONLY_DELETED)) {

	    /* Range */
	       for (i = p->low; i <= p->high; i++) {
		    if (i == 0)
			 continue;

		    dsc_get_trn_info3 (&dsc_public.nb, i, &t_info, &result);
		    if ((result != DELETED_TRN) || (t_info.current != 0)) {
		      t_info.current = i;
		      filter_call_func(func, &t_info, &result, filter_flags);
		    }
		    dsc_destroy_trn_info3(&t_info);
		    if (result != 0)
			 return(result);

		    if (interrupt)
			 goto exit;
	       }
	  } else {					/* Walk chains where possible */
	       trn_no = p->low;
	       if (trn_no < dsc_public.m_info.first)
		    trn_no = dsc_public.m_info.first;

	       while (trn_no != 0 && trn_no <= p->high) {
		    if (interrupt)
			 goto exit;

		    dsc_get_trn_info3(&dsc_public.nb, trn_no, &t_info, &result);
		    t_info.current = trn_no;

		    if (result == DELETED_TRN) {
			 trn_no++;
			 dsc_destroy_trn_info3(&t_info);
			 continue;
		    }

		    filter_call_func(func, &t_info, &result, filter_flags);
		    dsc_destroy_trn_info3(&t_info);
		    if (result != 0)
			 return(result);

		    trn_no = t_info.next;
	       }
	  }
     }
 exit:
     dont_flag_interrupts();
     return(0);
}

selection_list *
trn_select(t_info, string, old_sl_ptr, code_ptr)
	trn_info *t_info;
	char *string;
	selection_list *old_sl_ptr;
	register int *code_ptr;
{
	int low, high, flags;

	*code_ptr = trnexpr_parse(&dsc_public.m_info, t_info, string,
				  &low, &high, &flags);
	if (*code_ptr != 0)
		return((selection_list *)NULL);
	old_sl_ptr = sl_insert_range(low, high, flags, old_sl_ptr, code_ptr);
	return(old_sl_ptr);
}
