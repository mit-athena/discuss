/*
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_select.c,v 1.4 1986-10-14 23:00:04 spook Exp $
 * $Locker:  $
 *
 */

#define	MIN(a,b)	((a)<(b)?(a):(b))
#define	MAX(a,b)	((a)>(b)?(a):(b))
#include <stdio.h>
#include "../include/interface.h"
#include "../include/discuss_err.h"
#include "globals.h"

extern char *malloc();

selection_list *
sl_insert_range(low, high, old_list, code_ptr)
	register int low, high;
	register selection_list *old_list;
	error_code *code_ptr;
{
	register selection_list *p, *new;
	p = old_list;

	new = (selection_list *)malloc(sizeof(selection_list));
	if (!new) {
		*code_ptr = ERRNO;
		return((selection_list *)NULL);
	}
	new->low = low;
	new->high = high;
	new->next = (selection_list *)NULL;

	if (!p)
		return(new);

	while (p) {
		if (high-1 < p->low)
			p = p->next;
		if (high > p->low)
			break;
	}

	while (1) {
		if (p->next = (selection_list *)NULL)
			break;
		else if (p->high < p->next->low - 1)
			break;
		else {
			register selection_list *q;
			p->high = MAX(p->high, p->next->high);
			q = p->next;
			p->next = p->next->next;
			free((char *)q);
		}
	}
}

selection_list *
sl_insert_num(num, old_list, code_ptr)
	int num;
	selection_list *old_list;
	error_code *code_ptr;
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

	for (p = list; p; p = p->next) {
		for (i = p->low; i <= p->high; i++) {
			if (i == 0)
				continue;
			if ((result = func(i))) {
				return(result);
			}
		}
	}
	return(0);
}

selection_list *
trn_select(t_info, string, old_sl_ptr, code_ptr)
	trn_info *t_info;
	char *string;
	selection_list *old_sl_ptr;
	register error_code *code_ptr;
{
	int low, high;

	*code_ptr = trnexpr_parse(&m_info, t_info, string, &low, &high);
	if (*code_ptr != 0)
		return((selection_list *)NULL);
	old_sl_ptr = sl_insert_range(low, high, old_sl_ptr, code_ptr);
	return(old_sl_ptr);
}
