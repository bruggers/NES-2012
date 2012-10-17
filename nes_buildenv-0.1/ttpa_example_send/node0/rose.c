#include "ttpa_rose.h"

TTPA_ROSEFILE()
{
	//use TTPA_ROSESEC2
	TTPA_ROSESTATUS(0x00),
	TTPA_ROSESEC2() {
		//MSA round
		TTPA_ROSEENTRY(0x05, 1),
		//"our" round
		TTPA_ROSEENTRY(0x02, 1),
		//MSD round and end-of-rounds-flag set
		TTPA_ROSEENTRY_EORS(0x01, 1),
	},
	//copy of TTPA_ROSESEC2()
	TTPA_ROSESEC3() {
		TTPA_ROSEENTRY(0x05, 1),
		TTPA_ROSEENTRY(0x02, 1),
		TTPA_ROSEENTRY_EORS(0x01, 1),
	}
};
