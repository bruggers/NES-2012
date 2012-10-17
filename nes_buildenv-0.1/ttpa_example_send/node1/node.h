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

#include "ttpa.h"
#include "ifs.h"
#include "ifs_types.h"

#if defined (__AVR_ATmega128__)

#define PORT_BARGRAPH PORTD
#define DDR_BARGRAPH DDRD

#else
#  error "Wrong Target!\n"
#endif

//macros for filename and location of the application file
#define APPL_FN        0x30
#define APPL_SEC ifs_int_eep

//definition of applfile
struct applfile_struct {
     ifs_addr_t fileaddr;
     ifs_uint8_t error;
};

//return value for the background task
#define STATUS_OK 0

//macros for filename and location of the I/O file
#define IO_FN 0x20
#define IO_SEC ifs_int_0

//definition of I/O file, only a 8 bit integer is needed for our purposes
struct iofile_struct {
  ifs_uint8_t  counter_value;
};





