/*
 *
 * disserve.c -- Simple top level program for test.
 *
 */
#include <stdio.h>
extern char *error_message();

main ()			     		/* arguments, what arguments? */
{
int code;

     init_rpc("discuss",&code);
     if (code) {
	fprintf(stderr, "%s\n", error_message(code));
	exit(1);
     }
     while (!code)
	  recvit (&code);

}
