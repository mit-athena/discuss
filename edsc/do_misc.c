/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * do_misc.c -- Routines to implement the miscellaneous lisp requests 
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#include <ctype.h>
#include <sys/time.h>
#include <netdb.h>
#include "edsc.h"

#define	MIN(a,b)	((a)<(b)?(a):(b))

extern char	*edsc_protocol_version, *edsc_version_string;


do_gpv(args)
	char	*args;
{
	printf("(%s \"%s\")\n", edsc_protocol_version, edsc_version_string,
	       edsc_protocol_version);
}

int use_vectors;

typedef struct {
	char *val;
	int ok;
} gotten_word;

do_set (args)
	char	*args;
{
	char *varname, *value, *tmp, delim;

	if (get_word (&args, &tmp, " ", &delim) < 0) {
		printf ("; Missing variable name\n");
		return;
	}
	varname = tmp;
	if (get_word (&args, &tmp, ")", &delim) < 0) {
		printf ("; Missing value\n");
		return;
	}
	value = tmp;
	if (args[2]) {
		printf ("; Extra parameters supplied\n");
		return;
	}
	if (!strcmp (varname, "use_vectors"))
		use_vectors = atoi (value);
}

do_pacl(args)
	char	*args;
{
	char *cp = args, *mtg_name, delim, *filename;
	name_blk nb;
	dsc_acl	*list;
	int	code,n;
	register dsc_acl_entry *ae;
	FILE	*f;

	/* First, we get the output filename */
	if (get_word(&cp, &filename, " ", &delim) < 0) {
		printf(";Missing filename\n");
		return;
	}

	if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
		printf(";Missing meeting name\n");
		return;
	}

	(void) chmod(filename, 0600);
	if ((f = fopen(filename, "w")) == NULL) {
		printf(";%s\n", error_message(errno));
		return;
	}

	dsc_get_mtg (user_id, mtg_name, &nb, &code);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		fclose(f);
		return;
	}

	dsc_get_acl(&nb, &code, &list);
	if (code) {
		printf(";Can't read ACL: %s\n", error_message(code));
		fclose(f);
		return;
	}
	for (ae = list->acl_entries, n = list->acl_length; n; --n, ++ae)
		fprintf(f, "%-10s %s\n", ae->modes, ae->principal);
	acl_destroy(list);

	fclose(f);
	dsc_destroy_name_blk(&nb);
	printf("()\n");
}

static char	edsc_principal_buf[120];

char *fix_principal(principal)
	char	*principal;
{
	strcpy(edsc_principal_buf, principal);
	if (strcmp(edsc_principal_buf,"*") != 0 &&
	    index(edsc_principal_buf, '@') == 0) {
		strcat(edsc_principal_buf, "@");
		strcat(edsc_principal_buf, local_realm());
	}
	return(edsc_principal_buf);
}

do_dacl(args)
	char	*args;
{
	char *cp = args, *mtg_name, delim, *principal;
	name_blk nb;
	int	code;
	
	if (get_word(&cp, &principal, " ", &delim) < 0) {
		printf(";Missing principal\n");
		return;
	}

	if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
		printf(";Missing meeting name\n");
		return;
	}

	dsc_get_mtg (user_id, mtg_name, &nb, &code);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		return;
	}

	dsc_delete_access(&nb, fix_principal(principal), &code);
	dsc_destroy_name_blk(&nb);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		return;
	}

	printf("()\n");
}

do_sacl(args)
	char	*args;
{
	char *cp = args, *mtg_name, delim, *principal, *modes;
	register char	*p;
	name_blk nb;
	int	code;
	
	/* First, we get the mode bits */
	if (get_word(&cp, &modes, " ", &delim) < 0) {
		printf(";Missing ACL modes\n");
		return;
	}
	
	if (modes[0] && (modes[0] == '\"') &&
	    (modes[strlen(modes)-1] == '\"')) {
		for (p=modes+1; *(p+1); p++)
			*(p-1) = *p;
		*(p-1) = '\0';
	}
	
	if (get_word(&cp, &principal, " ", &delim) < 0) {
		printf(";Missing principal\n");
		return;
	}

	if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
		printf(";Missing meeting name\n");
		return;
	}

	dsc_get_mtg (user_id, mtg_name, &nb, &code);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		return;
	}

	dsc_set_access(&nb, fix_principal(principal), modes, &code);
	dsc_destroy_name_blk(&nb);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		return;
	}

	printf("()\n");
}

static char buffer[1024];

do_ls(args)
	char	*args;
{
	char *cp = args, *mtg_name, delim, *filename;
	char *low_string, *high_string, *filter_string;
	name_blk nb;
	trn_info2 t_info;
	mtg_info m_info;
	int	code, flags;
	FILE	*f;
	trn_nums trn_num, low, high;
	int	num_printed = 0;

	/* First, we get the output filename */
	if (get_word(&cp, &filename, " ", &delim) < 0) {
		printf(";Missing filename\n");
		return;
	}

	if (get_word(&cp, &low_string, " ", &delim) < 0) {
		printf(";Missing lower limit\n");
		return;
	}

	if (get_word(&cp, &high_string, " ", &delim) < 0) {
		printf(";Missing upper limit\n");
		return;
	}

	if (get_word(&cp, &filter_string, " ", &delim) < 0) {
		printf(";Missing filter flags\n");
		return;
	}

	if (get_word(&cp, &mtg_name, ")", &delim) < 0) {
		printf(";Missing meeting name\n");
		return;
	}
	low = atoi(low_string);
	high= atoi(high_string);
	flags = atoi(filter_string);

	if ((f = fopen(filename, "w")) == NULL) {
		printf(";%s\n", error_message(errno));
		return;
	}
	(void) chmod(filename, 0600);
	
	dsc_get_mtg (user_id, mtg_name, &nb, &code);
	if (code != 0) {
		printf(";%s\n", error_message(code));
		fclose(f);
		return;
	}
	
	trn_num = low;
	if (flags & flag_AREF) {
		dsc_get_trn_info2 (&nb, trn_num, &t_info, &code);
		if (code != 0) {
			printf(";[%04d]: %s\n", trn_num, error_message(code));
			dsc_destroy_name_blk(&nb);
			fclose(f);	
			return;
		}
		trn_num = t_info.fref;
		dsc_destroy_trn_info(&t_info);
	} else {
		dsc_get_mtg_info(&nb,&m_info,&code);
		if (code != 0) {
			printf(";%s\n", error_message(code));
			dsc_destroy_name_blk(&nb);
			fclose(f);
			return;
		}
		if (trn_num < m_info.first)
			trn_num = m_info.first;
	}
	
	while (trn_num) {
		dsc_get_trn_info2 (&nb, trn_num, &t_info, &code);
		if (code == DELETED_TRN) {
			if (!(flags & (filter_INCLUDE_DELETED |
				      filter_ONLY_DELETED))) {
				trn_num++;
				dsc_destroy_trn_info(&t_info);
				if ((trn_num > high) || (flags & flag_AREF))
					break;
				continue;
			}
		} else if (code) {
			printf("-[%04d]: %s\n", trn_num, error_message(code));
			goto loop_end;
		}
		if (flags & filter_ONLY_DELETED) {
			if (!(t_info.flags & TRN_FDELETED))
				goto loop_end;
		} else if (!(flags & filter_INCLUDE_DELETED))
			if (t_info.flags & TRN_FDELETED)
				goto loop_end;
		if (flags & filter_ONLY_INITIAL)
			if (t_info.pref)
				goto loop_end;
		if (flags & filter_ONLY_TERMINAL)
			if (t_info.nref)
				goto loop_end;
		if (flags & filter_FLAG_SET)
			if (!(t_info.flags & TRN_FLAG1))
				goto loop_end;
		if (flags & filter_FLAG_RESET)
			if (t_info.flags & TRN_FLAG1)
				goto loop_end;
		list_it(&t_info,f, 1);
		num_printed++;
	loop_end:
		if (flags &
		    (filter_INCLUDE_DELETED | filter_ONLY_DELETED))
			trn_num++;
		else if (flags & flag_AREF)
			trn_num = t_info.nref;
		else
			trn_num = t_info.next;
		if (!(flags & flag_AREF))
			if (trn_num > high)
				trn_num = 0;
		dsc_destroy_trn_info(&t_info);
	}
	fclose(f);
	dsc_destroy_name_blk(&nb);
	printf("(%d)\n", num_printed);
}



static list_it(t_infop, f, long_subjects)
	trn_info2 *t_infop;
	FILE	*f;
	int	long_subjects;
{
	char newtime[26], nlines[10];
	char *cp;
	int len;

	strcpy(newtime, short_time(&t_infop->date_entered));
	/*
	 * If author ends with current realm, punt the realm.
	 */
	if ((cp=index(t_infop->author, '@')) != NULL)
		if (!strcmp(cp+1, local_realm()))
			*cp = '\0';

	(void) sprintf (nlines, "(%d)", t_infop->num_lines);
	(void) sprintf (buffer, " [%04d]%c%c",
			t_infop->current,
			((t_infop->flags & TRN_FLAG1) != 0) ? 'F' : ' ',
			((t_infop->flags & TRN_FDELETED) != 0) ? 'D' : ' ');
	if ((len = MIN(5, 13-strlen (buffer)) - strlen (nlines)) > 0)
		(void) strncat (buffer, "     ", len);

	if (strlen(t_infop->author) > 15)
		(void) strcpy(&t_infop->author[12], "...");

	(void) sprintf (buffer + strlen (buffer), "%s %s %-15s ",
			nlines, newtime, t_infop->author);
	len = 79 - strlen (buffer);

	if (!long_subjects && strlen (t_infop->subject) > len)
	    (void) strcpy (&t_infop->subject [len - 3], "...");

	(void) fprintf (f, "%s%s\n", buffer, t_infop->subject);

}

