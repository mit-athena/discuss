/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
#include <stdio.h>
#include <discuss/discuss.h>
#include <ss/ss.h>
#include "globals.h"

struct _dsc_pub dsc_public = { 0, 0, 0, (char *)NULL, (char *)NULL };
char	*temp_file = (char *)NULL;
char	*pgm = (char *)NULL;
char	*user_id = (char *)NULL;
