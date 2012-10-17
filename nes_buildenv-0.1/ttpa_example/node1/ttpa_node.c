/* Copyright (c) 2004, Christian Troedhandl
   All rights reserved.
 
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
 
   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.
   * Neither the name of the copyright holders nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.
 
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE. */

/*
 * appl.c
 *      Example application for TTP/A
 *
 * The task reads the IFS address from the first entry in the application
 * file and opens the corresponding file. It then reads a 8 bit integer
 * from the opened file and adds 1 to it.
 * The result is written back to the file.
 *
 */
#include <stdio.h>
#include "ttpa.h"
#include "ifs.h"
#include "ifs_types.h"
#include "schedule.h"

#include "ttpa_node.h"
#include "node1.h"


struct applfile_struct IFS_LOC(APPL_SEC) applfile = {
	IFS_ADDR_S(IO_FN, 0x01, 0x00), 1, 0};

struct iofile_struct IFS_LOC(IO_SEC) iofile;


IFS_ADDAPPLFILE(IO_FN, &iofile, NULL, IFS_FILELEN(struct iofile_struct), IO_SEC, 066);

void mytask(ttpa_taskparam_t param)
{
	ifs_fd_t io_fd;
	uint8_t data;


	if(ifs_open(&io_fd, applfile.fileaddr, IFS_REQ(IFS_ACCESS_APPL,IFS_OP_RW,2)) ==
	   IFS_ERR_SUCC) {
		data = ifs_rd_i8(&io_fd, 0);

		if(io_fd.error == IFS_ERR_SUCC) {
			PORTA = ~data;

			data += 1;

			if(ifs_wr_i8(&io_fd, 0, data) == IFS_ERR_SUCC) {
				applfile.error = UINT8_TO_IFS(0);
			} else {
				applfile.error = UINT8_TO_IFS(1);
			}
		} else {
			applfile.error = UINT8_TO_IFS(1);
		}
	}
	ifs_close(&io_fd);
}
IFS_ADDAPPLFILE(APPL_FN, &applfile, mytask, IFS_FILELEN(struct applfile_struct), APPL_SEC, 077);

		
/*
 * initialize fan
 */
int appl_fan_init(void);

int appl_fan_init(void)
{
	// turn off fans
	VENT_OFF();
	
	//enable leds
	DDRA = 0xFF;
	
	return 0;

}

ADD_INITTASK(task_fan_init, appl_fan_init, 9, (1<<TTPA_STATE_UNSYNC));
