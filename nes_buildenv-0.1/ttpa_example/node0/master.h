/* Copyright (c) 2004, Christian TrÃ¶dhandl
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
 * appl.h
 *      Example application for TTP/A (definitions)
 *
 */
#include "ttpa.h"
#include "ifs.h"
#include "ifs_types.h"

#include <avr/io.h>
#include <stdio.h>
#include <avr/pgmspace.h>
#include <avr/interrupt.h>
#include <avr/signal.h>




#if defined (__AVR_ATmega128__)

#  define CPU_CLK       14745600
#  define COM_BAUDRATE     19200
#  define BAUDRATE_MAGIC ((CPU_CLK >> 4) / COM_BAUDRATE - 1)

/* 19200 8E1, TX enable, RX disable					*/
#  define MY_UART_CONTROL_A		UCSR1A
#  define MY_UART_CONTROL_B		UCSR1B
#  define MY_UART_CONTROL_C		UCSR1C
#  define MY_UART_DATA_REGISTER		UDR1
#  define MY_UART_BAUDRATE_H		UBRR1H
#  define MY_UART_BAUDRATE_L		UBRR1L
#  define MY_UART_CONTROL_A_INIT	0x00
#  define MY_UART_CONTROL_B_INIT	0x18
#  define MY_UART_CONTROL_C_INIT	0x26
#  define MY_UART_BAUDRATE_H_INIT	(BAUDRATE_MAGIC >> 8)
#  define MY_UART_BAUDRATE_L_INIT	(BAUDRATE_MAGIC & 0xFF)
#  define MY_UART_CONTROL_UDRE		UDRE


#  define HWUART1_OPORT	PORTD
#  define HWUART1_IPORT	PIND
#  define HWUART1_DDR   DDRD
#  define HWUART1_RXD	PD2
#  define HWUART1_TXD	PD3

#else
#  error "Wrong Target!\n"
#endif




#define APPL_FN 0x30
#define APPL_SEC ifs_int_eep

struct applfile_struct {
	ifs_addr_t fileaddr; // IFS address for IO file
	ifs_int8_t inc;      // increment value
	ifs_uint8_t error;   // error flag (0: no error)
};

#define IO_FN 0x20
#define IO_SEC ifs_int_0

struct iofile_struct {
	ifs_int8_t value;
};

extern void mytask(ttpa_taskparam_t param);


