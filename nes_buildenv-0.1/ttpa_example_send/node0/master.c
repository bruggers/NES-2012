#include <stdio.h>
#include "ttpa.h"
#include "ifs.h"
#include "ifs_types.h"
#include "schedule.h"

#include "master.h"

//initialize both IFS-files, applfile.fileaddr is set to IO_FN, record 0x01, byte 0x00(=counter value), error flag is set to 0
struct applfile_struct IFS_LOC(APPL_SEC) applfile = {IFS_ADDR_S(IO_FN, 0x01, 0x00), 0};
struct iofile_struct IFS_LOC(IO_SEC) iofile;

IFS_ADDAPPLFILE(IO_FN, &iofile, NULL, IFS_FILELEN(struct iofile_struct), IO_SEC, 066);
IFS_ADDAPPLFILE(APPL_FN, &applfile, task_counter, IFS_FILELEN(struct applfile_struct), APPL_SEC, 077);

//TTP/A task
void task_counter(ttpa_taskparam_t param)
{
	//file-descriptor for working with the I/O file.
	ifs_fd_t io_fd;
	//for the address of the I/O-file
	ifs_addr_t io_addr;
	//the counter value
	uint8_t counter_value;

	//read the address of the I/O-file
	io_addr=applfile.fileaddr;
	
	//open the I/O-file
	if(ifs_open(&io_fd, io_addr, IFS_REQ(IFS_ACCESS_APPL,IFS_OP_RW,1)) ==  IFS_ERR_SUCC) 
	{
		//read the counter value from the I/O-file, parameter 0 because it is a 8 bit value
		counter_value=ifs_rd_u8(&io_fd, 0);
		if(io_fd.error == IFS_ERR_SUCC) 
		{
			//increase the counter, watch for overflow
			if (counter_value<255)
			{
				counter_value++;
			}
			else
			{
				counter_value=0;
			}
				
			//write the counter value to the I/O file, parameter 0 because it is a 8 bit value	
			if(ifs_wr_u8(&io_fd, 0, counter_value) == IFS_ERR_SUCC) 
		 	{       
		    		applfile.error = UINT8_TO_IFS(0);
		  	} 
			else 
		  	{
		    		applfile.error = UINT8_TO_IFS(1);
		  	}
		}
		else 
		{
			applfile.error = UINT8_TO_IFS(1);
	  	}
	  }
	  
	//close the I/O file
	ifs_close(&io_fd);
};

