#include <stdio.h>
#include <sys/file.h>
#include <strings.h>
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"

extern ss_request_table discuss_cmds;
int current_trans = -1;
char *cur_mtg = (char *)NULL;
int l_zcode;
char *temp_file = (char *)NULL;
char *pgm = (char *)NULL;
char *malloc(), *getenv(), *gets(), *ctime();
mtg_info m_info;
char buffer[BUFSIZ];
int time_now, time_sixmonthsago;
main(argc, argv)
	int argc;
	char **argv;
{
	CODE zcode;
	int sci;

	time(&time_now); 
	time_sixmonthsago = time_now - 6*30*24*60*60; 

	sci = ss_create_invocation("discuss", "0.1", (char *)NULL,
				   &discuss_cmds, 
				   "/projects/discuss/client/info", &zcode);
	if (zcode != (CODE)0) {
		ss_perror(sci, zcode, "creating invocation");
		exit(1);
	}
	temp_file = malloc(64);
	pgm = malloc(64);
	sprintf(temp_file, "/tmp/mtg%d.%d", getuid(), getpid());
	init_rpc();
	if (argc != 1) {
		sprintf(buffer, "goto %s", argv[1]);
		ss_execute_line(sci, buffer, &l_zcode);
		if (l_zcode != 0) fprintf(stderr, "Error code %d.\n", l_zcode);
	}
	ss_listen (sci, &zcode);
	unlink(temp_file);
}

edit(fn)
	char *fn;
{
	char *ed = getenv("EDITOR");
	if (!ed) ed="/bin/ed";

	switch(fork()) {
	case -1:
		perror("couldn't fork");
		break;
	case 0:
		execlp(ed, ed, fn, 0);
		perror(ed);
		exit(1);
		break;
	default:
		break;
	}
	wait(0);
}

new_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int fd, txn_no;
	tfile tf;
	char *subject = &buffer[0];

	if (cur_mtg == (char *)NULL) {
		fprintf(stderr, "Not currently attending a meeting.\n");
		ss_abort_line(sci_idx);
	}
	if (argc != 1) {
		fprintf(stderr, "Usage:  %s\n", argv[0]);
		ss_abort_line(sci_idx);
	}
	printf("Subject: ");
	if (gets(subject) == (char *)NULL) {
		fprintf(stderr, "Error reading subject.\n");
		ss_abort_line(sci_idx);
	}
	unlink(temp_file);
	edit(temp_file);
	fd = open(temp_file, O_RDONLY, 0);
	if (fd < 0) {
		fprintf(stderr, "No file; not entered.\n");
		return;
	}
	tf = unix_tfile(fd);
	add_trn(cur_mtg, tf, subject, 0, &txn_no, &l_zcode);
	if (l_zcode != 0) {
		fprintf(stderr, "Error %d.\n", l_zcode);
		ss_abort_line(sci_idx);
	}
	printf("Transaction [%04d] entered in the %s meeting.\n",
	       txn_no, cur_mtg);
	current_trans = txn_no;
}

repl(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int fd, txn_no;
	tfile tf;
	trn_info t_info;

	if (cur_mtg == (char *)NULL) {
		fprintf(stderr, "Not currently attending a meeting.\n");
		ss_abort_line(sci_idx);
	}
	if (argc != 1) {
		fprintf(stderr, "Usage:  %s\n", argv[0]);
		ss_abort_line(sci_idx);
	}
	if (current_trans == -1) {
		fprintf(stderr, "No current transaction.\n");
		ss_abort_line(sci_idx);
	}
	get_trn_info(cur_mtg,current_trans, &t_info, &l_zcode);
	if (l_zcode != 0) {
		fprintf(stderr,
			"Can't get info on current transaction.  Error %d.\n",
			l_zcode);
		ss_abort_line(sci_idx);
	}
	if (strncmp(t_info.subject, "Re: ", 4)) {
		char *new_subject = malloc(strlen(t_info.subject) + 5);
		strcpy(new_subject, "Re: ");
		strcat(new_subject, t_info.subject);
		free(t_info.subject);
		t_info.subject = new_subject;
	}
	unlink(temp_file);
	edit(temp_file);
	fd = open(temp_file, O_RDONLY, 0);
	if (fd < 0) {
		fprintf(stderr, "No file; not entered.\n");
		return;
	}
	tf = unix_tfile(fd);
	
	add_trn(cur_mtg, tf, t_info.subject,
		current_trans, &txn_no, &l_zcode);
	if (l_zcode != 0) {
		fprintf(stderr, "Error %d.\n", l_zcode);
		ss_abort_line(sci_idx);
	}
	printf("Transaction [%04d] entered in the %s meeting.\n",
	       txn_no, cur_mtg);
	current_trans = txn_no;
}

del_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	if (cur_mtg == (char *)NULL) {
		printf("No current meeting.\n");
		ss_abort_line(sci_idx);
	}
	if (argc != 2) {
		fprintf(stderr, "Usage:  %s trn_no\n", argv[0]);
		ss_abort_line(sci_idx);
	}
	txn_no = atoi(argv[1]);
	delete_trn(cur_mtg, txn_no, &l_zcode);
	if (l_zcode != 0) {
		fprintf(stderr, "Error %d.\n", l_zcode);
		ss_abort_line(sci_idx);
	}
	current_trans = txn_no + 1;
}

ret_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	if (cur_mtg == (char *)NULL) {
		printf("No current meeting.\n");
		ss_abort_line(sci_idx);
	}
	if (argc != 2) {
		fprintf(stderr, "Usage:  %s trn_no\n", argv[0]);
		ss_abort_line(sci_idx);
	}
	txn_no = atoi(argv[1]);
	retrieve_trn(cur_mtg, txn_no, &l_zcode);
	if (l_zcode != 0) {
		fprintf(stderr, "Error %d.\n", l_zcode);
		ss_abort_line(sci_idx);
	}
	current_trans = txn_no;
}

prt_trans(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	tfile tf;
	int fd, pid;

	if (cur_mtg == (char *)NULL) {
		printf("No current meeting.\n");
		ss_abort_line(sci_idx);
	}
	if (argc != 2) {
		fprintf(stderr, "Usage:  %s trn_no\n", argv[0]);
		ss_abort_line(sci_idx);
	}
	txn_no = atoi(argv[1]);
	current_trans = txn_no;
	fd = pager_create();
	if (fd < 0) {
		ss_perror(sci_idx, ERRNO, "Can't start pager");
		ss_abort_line(sci_idx);
	}
	tf = unix_tfile(fd);
	write_trans(sci_idx, txn_no, tf);
	tclose(tf);
	close(fd);
	tdestroy(tf);
	wait(0);
}

goto_mtg(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	char *path;

	if (argc != 2) {
		fprintf(stderr, "Usage:  %s mtg_name\n", argv[0]);
		ss_abort_line(sci_idx);
	}
	if (cur_mtg != (char *)NULL) free(cur_mtg);
	cur_mtg = (char *)NULL;
	path = malloc(strlen(argv[1]) + 20 * sizeof(char));
	strcpy(path, "/usr/spool/discuss/");
	strcat(path, argv[1]);
	get_mtg_info(path, &m_info, &l_zcode);
	if (l_zcode != 0) {
		if (l_zcode == NO_SUCH_MTG)
			fprintf(stderr, "No such meeting. %s\n", argv[1]);
		else if (l_zcode == BAD_MTG_NAME)
			fprintf(stderr, "Bad meeting name. %s\n", argv[1]);
		else
			fprintf(stderr, "Error %d.\n", l_zcode);
		ss_abort_line(sci_idx);
	}
	cur_mtg = path;
}

list(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	int txn_no;
	trn_info t_info;
	char newtime[26];
	char *cp;

	if (argc != 1) {
		fprintf(stderr, "Usage:  %s\n", argv[0]);
		ss_abort_line(sci_idx);
	}
	if (cur_mtg == (char *)NULL) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		ss_abort_line(sci_idx);
	}
	txn_no = m_info.first;
	while (txn_no != 0) {
		get_trn_info(cur_mtg, txn_no, &t_info, &l_zcode);
		if (l_zcode != 0) {
			if (l_zcode != DELETED_TRN)
				fprintf(stderr,
					"Cannot get trn #%d info, code %d.\n",
					txn_no, l_zcode);
			break;
		}
		cp=ctime(&t_info.date_entered);
		if((t_info.date_entered < time_sixmonthsago) ||
		   (t_info.date_entered > time_now))
			sprintf(newtime, "%-7.7s %-4.4s", cp+4, cp+20);
		else
			sprintf(newtime, "%-12.12s", cp+4);
		/*
		 * If author ends with current realm, punt the realm.
		 */
		if (((cp=index(t_info.author, '@')) != NULL) &&
		    (!strcmp(cp+1, REALM))) {
			*cp = '\0';
		}
		if (strlen(t_info.author) > 15) {
			t_info.author[15] = '\0';
			t_info.author[14] = '.';
			t_info.author[13] = '.';
			t_info.author[12] = '.';
		}
		sprintf(buffer, "(%d)", t_info.num_lines);
		printf(" [%04d]%c%5s %s %-15s %s\n",
		       t_info.current,
		       (t_info.current == current_trans) ? '*' : ' ',
		       buffer,
		       newtime,
		       t_info.author,
		       t_info.subject);
		txn_no = t_info.next;
		free (t_info.author);
		free (t_info.subject);
	}
}
