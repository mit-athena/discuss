#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include <pwd.h>

struct passwd *pwd;
char pathname[MAXPATHLEN];
int fd;

char *text = "0:0:0:charon.mit.edu:/var/spool/discuss/new_meetings:New_Meetings,new_meetings:\n0:541394699:24:charon.mit.edu:/var/spool/discuss/eve:Everybody,eve:\n";

#ifndef	lint
static char rcsid_dsc_setup_c[] = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/mclient/dsc_setup.c,v 1.2 1998-03-24 22:26:22 ghudson Exp $";
#endif

main()
{
	int len = strlen(text);
	pwd = getpwuid(getuid());
	if (!pwd) {
		fprintf(stderr, "Who are you?\n");
		exit(1);
	}
	strcpy(pathname, pwd->pw_dir);
	strcat(pathname, "/.meetings");
	fd = open(pathname, O_RDWR|O_CREAT|O_EXCL, 0666);
	if (fd < 0) {
		perror(pathname);
		exit(1);
	}
	if (write(fd, text, len) != len) {
		perror("write failed");
		unlink(pathname);
		exit(1);
	}
	close(fd);
}
