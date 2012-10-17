#include <stdio.h>
#include "ttpa.h"
#include "ifs.h"
#include "ifs_types.h"
#include "schedule.h"

#include "node.h"

//global variable for outputting the counter in a background task
uint8_t counter_value;

//initialize both IFS-files, applfile.fileaddr is set to IO_FN, record 0x01, byte 0x00(=counter value), error flag is set to 0
struct iofile_struct IFS_LOC(IO_SEC) iofile;
struct applfile_struct IFS_LOC(APPL_SEC) applfile = {IFS_ADDR_S(IO_FN, 0x01, 0x00), 0};

void task_read_counter(ttpa_taskparam_t param);

IFS_ADDAPPLFILE(IO_FN, &iofile, NULL, IFS_FILELEN(struct iofile_struct), IO_SEC, 066);
IFS_ADDAPPLFILE(APPL_FN, &applfile, task_read_counter, IFS_FILELEN(struct applfile_struct), APPL_SEC, 077);

//TTP/A task for reading the counter value from the IFS
void task_read_counter(ttpa_taskparam_t param)
{
	//file-descriptor for working with the I/O file.
  	ifs_fd_t io_fd;
  	//for the address of the I/O-file
	ifs_addr_t io_addr;
	//open the I/O-file
	io_addr = applfile.fileaddr;

	//open the I/O-file
	if(ifs_open(&io_fd, io_addr, IFS_REQ(IFS_ACCESS_APPL,IFS_OP_RW,1)) == IFS_ERR_SUCC) 
	{
		//save the value from the IFS to a global variable to use it in the background task
	 	counter_value = ifs_rd_u8(&io_fd, 0);
	}
	//close the I/O file
	ifs_close(&io_fd);
}

//background task to output the value on the bargraph
int counter_to_bargraph(void)
{
	DDR_BARGRAPH|=0xFF;
	PORT_BARGRAPH=counter_value;

	//return 0 to reschedule
 	return (STATUS_OK);
}
//add background task
ADD_BGTASK(task_counter_to_bargraph, counter_to_bargraph, 2, (1<<TTPA_STATE_ACTIVE));
