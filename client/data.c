#include <stdio.h>
#include "types.h"
#include "interface.h"
#include "ss.h"
#include "globals.h"

struct _dsc_pub dsc_public = { 0, 0, 0, (char *)NULL, (char *)NULL };
char	*temp_file = (char *)NULL;
char	*pgm = (char *)NULL;
char	*user_id = (char *)NULL;
