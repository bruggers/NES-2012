#include <stdio.h>
#include "ifs.h"
#include "ifs_types.h"
#include "ifs_rodl.h"
#include "ttpa_task.h"

#include "node.h"

IFS_RODLFILE(0x02, 1, ifs_int_eep)
{
	//end-of-round in slot3
	IFS_RE_EOR(0x03)
};
