#include <stdio.h>
#include "ifs.h"
#include "ifs_types.h"
#include "ifs_rodl.h"
#include "ttpa_task.h"

#include "master.h"


IFS_RODLFILE(0x02, 3, ifs_int_eep)
{
	IFS_RE_RECV(0x01, IFS_ADDR_I(IO_FN,0x01,0x00), 0),
	IFS_RE_EXEC(0x02, IFS_ADDR_I(APPL_FN,0x01,0x00)),
	IFS_RE_EOR(0x03)
};
