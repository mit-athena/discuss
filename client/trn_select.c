/*
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_select.c,v 1.9 1987-07-17 00:48:49 srz Exp $
 * $Locker:  $
 *
 */
#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_select.c,v 1.9 1987-07-17 00:48:49 srz Exp $";
#endif lint

#define	MIN(a,b)	((a)<(b)?(a):(b))
#define	MAX(a,b)	((a)>(b)?(a):(b))
#include <stdio.h>
#include "interface.h"
#include "discuss_err.h"
#include "globals.h"

extern char *malloc();

selection_list *
sl_insert_range(low, high, old_list, code_ptr)
	register int low, high;
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
sl_insert_num(num, old_list, code_ptr)
	int num;
	selection_list *old_list;
	int *code_ptr;
{
	return(sl_insert_range(num, num, old_list, code_ptr));
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

int
sl_map(func, list)
	int (*func)();		/* return 0 -> continue */
	selection_list *list;
{
	register selection_list *p;
	register int i, result;

	flag_interrupts();
	for (p = list; p; p = p->next) {
		for (i = p->low; i <= p->high; i++) {
			if (i == 0)
				continue;
			if ((result = func(i))) {
				return(result);
			}
			if (interrupt)
				goto exit;
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
	int low, high;

	*code_ptr = trnexpr_parse(&dsc_public.m_info, t_info, string,
				  &low, &high);
	if (*code_ptr != 0)
		return((selection_list *)NULL);
	old_sl_ptr = sl_insert_range(low, high, old_sl_ptr, code_ptr);
	return(old_sl_ptr);
}
