#define	MIN(a,b)	((a)<(b)?(a):(b))
#include <stdio.h>
#include "../include/interface.h"
#include "discuss_err.h"
#include "globals.h"

int *chosen_trn_map = (int *)NULL;
int map_size = 0;

void
get_trn_map(code_ptr)
	int	*code_ptr;
{
	get_mtg_info(cur_mtg, &m_info, code_ptr);
	if (*code_ptr)
		return;

	if (chosen_trn_map != (int *)NULL)
		free((char *)chosen_trn_map);
	chosen_trn_map = (int *)malloc((unsigned)(sizeof(int)*m_info.last));
	if (chosen_trn_map == (int *)NULL) {
		*code_ptr = errno;
		map_size = 0;
		return;
	}
	map_size = m_info.last;
	*code_ptr = 0;
}

error_code
trn_select(t_info, string)
	trn_info *t_info;
	char	*string;
{
	int code, i;
	int low, high;

	code = trnexpr_parse(&m_info, t_info, string, &low, &high);
	if (code != 0)
		return(code);
	high = MIN(high, map_size);
	for (i = low; i <= MIN(high, m_info.last); i++)
		chosen_trn_map[i]++;
	return(0);
}
