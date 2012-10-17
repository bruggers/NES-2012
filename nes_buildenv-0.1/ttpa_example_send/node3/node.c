#include <stdio.h>
#include "ttpa.h"
#include "ifs.h"
#include "ifs_types.h"
#include "schedule.h"

#include "node.h"


//turns off the (annoying) fan with an init task
int appl_fan_init(void)
{
	// turn off fan
	PORTB &= ~((1<<PB6) | (1<<PB5));
	DDRB |= ((1<<PB6) | (1<<PB5));
	
	return (0);
}

ADD_INITTASK(task_fan_init, appl_fan_init, 9, (1<<TTPA_STATE_UNSYNC));
