#include <stdio.h>
#include "ifs.h"
#include "ifs_types.h"
#include "ifs_rodl.h"
#include "ttpa_task.h"

#include "master.h"

//RODL number 0x02, 3 entries and the memory location
IFS_RODLFILE(0x02, 3, ifs_int_eep)
{
	//execute TTP/A task with name APPL_FN(=task_counter) in slot 0
	IFS_RE_EXEC(0x01, IFS_ADDR_I(APPL_FN,0x01,0x00)),
	//send IFS value (1 byte) at location IO_FN, record 0x01, byte 0x00(=counter value) on the bus in slot 2
	IFS_RE_SEND(0x02, IFS_ADDR_I(IO_FN,0x01,0x00), 0),
	//end-of-round in slot3
	IFS_RE_EOR(0x03)
};
