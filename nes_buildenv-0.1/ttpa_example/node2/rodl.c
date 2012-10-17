/*
 * ESP example RODL file for slave0
 */

#include "ifs.h"
#include "ifs_types.h"
#include "ifs_rodl.h"
#include "ttpa_task.h"



IFS_RODLFILE(0x02, 1, ifs_int_eep)
{
	IFS_RE_EOR(0x03)
};

