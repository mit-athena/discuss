/* UNIX Unified Stream Protocol 

   Copyright 1986 by the Massachusetts Institute of Technology 
   See permission and disclaimer notice in file "notice.h" 
*/

/* Generally Useful Things */

typedef unsigned short Boolean;
typedef char Byte;

extern int  errno;

char    *calloc();

#define TRUE (Boolean) 1
#define FALSE (Boolean) 0
#define SUCCESS 1
#define ERROR   (-1)
#define max(a, b) (a > b ? a : b)
